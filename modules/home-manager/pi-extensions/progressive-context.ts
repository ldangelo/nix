import type { BuildSystemPromptOptions, ExtensionAPI } from "@earendil-works/pi-coding-agent";

const VALID_DISCLOSURE = new Set(["always", "summary", "lazy", "disabled"]);
const SMALL_CONTEXT_CHARS = 1800;
const MAX_SUMMARY_LINES = 24;

type Disclosure = "always" | "summary" | "lazy" | "disabled";

type ContextFile = NonNullable<BuildSystemPromptOptions["contextFiles"]>[number];

type ParsedContext = {
	path: string;
	content: string;
	body: string;
	frontmatter: Record<string, unknown>;
	disclose: Disclosure;
	summary: string;
	headings: string[];
	triggers: string[];
	estimatedTokens: number;
};

let latestContexts: ParsedContext[] = [];

function estimateTokens(text: string): number {
	return Math.ceil(text.length / 4);
}

function escapeXml(value: string): string {
	return value
		.replace(/&/g, "&amp;")
		.replace(/</g, "&lt;")
		.replace(/>/g, "&gt;")
		.replace(/"/g, "&quot;")
		.replace(/'/g, "&apos;");
}

function parseScalar(value: string): unknown {
	const trimmed = value.trim();
	if (trimmed === "true") return true;
	if (trimmed === "false") return false;
	if (trimmed.startsWith("[") && trimmed.endsWith("]")) {
		return trimmed
			.slice(1, -1)
			.split(",")
			.map((part) => part.trim().replace(/^['\"]|['\"]$/g, ""))
			.filter(Boolean);
	}
	return trimmed.replace(/^['\"]|['\"]$/g, "");
}

function parseFrontmatter(content: string): { frontmatter: Record<string, unknown>; body: string } {
	const normalized = content.replace(/\r\n/g, "\n").replace(/\r/g, "\n");
	if (!normalized.startsWith("---\n")) return { frontmatter: {}, body: normalized };
	const end = normalized.indexOf("\n---", 4);
	if (end < 0) return { frontmatter: {}, body: normalized };
	const raw = normalized.slice(4, end);
	const body = normalized.slice(end + 4).trimStart();
	const frontmatter: Record<string, unknown> = {};
	for (const line of raw.split("\n")) {
		const match = line.match(/^([A-Za-z0-9_-]+):\s*(.*)$/);
		if (!match) continue;
		frontmatter[match[1]] = parseScalar(match[2] ?? "");
	}
	return { frontmatter, body };
}

function extractHeadings(body: string): string[] {
	return body
		.split("\n")
		.map((line) => line.match(/^(#{1,4})\s+(.+)$/))
		.filter((match): match is RegExpMatchArray => Boolean(match))
		.slice(0, 32)
		.map((match) => `${match[1]} ${match[2].trim()}`);
}

function extractKeywords(text: string, path: string): string[] {
	const words = new Set<string>();
	for (const part of path.split(/[\/._-]+/)) {
		const word = part.toLowerCase();
		if (word.length >= 3) words.add(word);
	}
	const important = [
		"nix",
		"darwin",
		"home-manager",
		"home",
		"flake",
		"flakes",
		"linux",
		"secrets",
		"sops",
		"beads",
		"bd",
		"br",
		"git",
		"commit",
		"deploy",
		"test",
		"format",
		"overlay",
		"overlays",
		"pi",
		"extension",
		"skill",
	];
	const lower = text.toLowerCase();
	for (const word of important) {
		if (lower.includes(word)) words.add(word);
	}
	return Array.from(words).slice(0, 18);
}

function normalizeTriggers(value: unknown): string[] {
	if (Array.isArray(value)) return value.map(String).map((s) => s.trim()).filter(Boolean);
	if (typeof value === "string") return value.split(/[,\s]+/).map((s) => s.trim()).filter(Boolean);
	return [];
}

function resolveDisclosure(frontmatter: Record<string, unknown>, body: string): Disclosure {
	const raw = String(frontmatter.disclose ?? frontmatter.disclosure ?? "").toLowerCase();
	if (VALID_DISCLOSURE.has(raw)) return raw as Disclosure;
	if (body.length <= SMALL_CONTEXT_CHARS) return "always";
	return "summary";
}

function summarize(body: string, headings: string[], frontmatter: Record<string, unknown>): string {
	const explicit = frontmatter.summary;
	if (typeof explicit === "string" && explicit.trim()) return explicit.trim();
	const lines = body
		.split("\n")
		.map((line) => line.trim())
		.filter((line) => line && !line.startsWith("<!--"));
	const intro = lines.find((line) => !line.startsWith("#") && line.length > 20) ?? lines[0] ?? "Project context file.";
	const critical = lines
		.filter((line) => /\b(MUST|CRITICAL|MANDATORY|NEVER|ALWAYS|Rules:|Respond terse|Session Protocol|Before ending)\b/i.test(line))
		.slice(0, 12);
	const criticalText = critical.length > 0 ? `\nCritical excerpts:\n${critical.map((line) => `- ${line.slice(0, 220)}`).join("\n")}` : "";
	const headingText = headings.length > 0 ? `\nContains:\n${headings.slice(0, MAX_SUMMARY_LINES).map((h) => `- ${h}`).join("\n")}` : "";
	return `${intro.slice(0, 500)}${criticalText}${headingText}`;
}

function parseContext(file: ContextFile): ParsedContext {
	const { frontmatter, body } = parseFrontmatter(file.content);
	const headings = extractHeadings(body);
	const disclose = resolveDisclosure(frontmatter, body);
	const triggers = [...normalizeTriggers(frontmatter.triggers), ...extractKeywords(`${body}\n${headings.join("\n")}`, file.path)];
	return {
		path: file.path,
		content: file.content,
		body,
		frontmatter,
		disclose,
		summary: summarize(body, headings, frontmatter),
		headings,
		triggers: Array.from(new Set(triggers.map((t) => t.toLowerCase()))).slice(0, 24),
		estimatedTokens: estimateTokens(body),
	};
}

function scoreContext(ctx: ParsedContext, prompt: string): number {
	const lower = prompt.toLowerCase();
	let score = 0;
	for (const trigger of ctx.triggers) {
		if (trigger && lower.includes(trigger)) score += trigger.length > 4 ? 2 : 1;
	}
	const pathParts = ctx.path.toLowerCase().split(/[\/]/).filter(Boolean);
	for (const part of pathParts) {
		if (part.length > 2 && lower.includes(part)) score += 2;
	}
	return score;
}

function buildManifest(contexts: ParsedContext[], prompt: string): string {
	const ranked = contexts
		.map((ctx) => ({ ctx, score: scoreContext(ctx, prompt) }))
		.sort((a, b) => b.score - a.score || a.ctx.path.localeCompare(b.ctx.path));
	const relevant = ranked.filter((entry) => entry.score > 0).slice(0, 5);
	const lines: string[] = [
		"<available_context>",
		...contexts
			.filter((ctx) => ctx.disclose !== "disabled")
			.map(
				(ctx) =>
					`  <context path=\"${escapeXml(ctx.path)}\" disclose=\"${ctx.disclose}\" tokens=\"${ctx.estimatedTokens}\" triggers=\"${escapeXml(ctx.triggers.join(","))}\">${escapeXml(ctx.summary.split("\n")[0] ?? "")}</context>`,
			),
		"</available_context>",
	];
	if (relevant.length > 0) {
		lines.push(
			"",
			"Likely relevant context for current request:",
			...relevant.map(({ ctx }) => `- ${ctx.path} (use \`context_load\` or \`read\` if details needed)`),
		);
	}
	return lines.join("\n");
}

function buildProgressiveContext(contexts: ParsedContext[], prompt: string): string {
	const parts: string[] = [
		"## Project Context (Progressive Disclosure)",
		"",
		"Only compact context is loaded by default. Use `context_load` or `read` to load full files when task matches path, triggers, or likely-relevant list. Honor loaded context over summaries if conflict.",
		"",
		buildManifest(contexts, prompt),
	];
	for (const ctx of contexts) {
		if (ctx.disclose === "disabled" || ctx.disclose === "lazy") continue;
		parts.push("", `### ${ctx.path}`);
		if (ctx.disclose === "always") {
			parts.push(ctx.body.trim());
		} else {
			parts.push(ctx.summary.trim());
		}
	}
	return parts.join("\n").trim();
}

function defaultProjectContextBlock(contextFiles: ContextFile[]): string {
	if (contextFiles.length === 0) return "";
	let block = "\n\n# Project Context\n\n";
	block += "Project-specific instructions and guidelines:\n\n";
	for (const { path, content } of contextFiles) {
		block += `## ${path}\n\n${content}\n\n`;
	}
	return block;
}

function xmlProjectContextBlock(contextFiles: ContextFile[]): string {
	if (contextFiles.length === 0) return "";
	let block = "\n\n<project_context>\n\n";
	block += "Project-specific instructions and guidelines:\n\n";
	for (const { path, content } of contextFiles) {
		block += `<project_instructions path=\"${path}\">\n${content}\n</project_instructions>\n\n`;
	}
	block += "</project_context>\n";
	return block;
}

function removeOriginalContext(systemPrompt: string, contextFiles: ContextFile[]): string {
	let next = systemPrompt;
	for (const block of [defaultProjectContextBlock(contextFiles), xmlProjectContextBlock(contextFiles)]) {
		if (block && next.includes(block)) next = next.replace(block, "\n\n");
	}
	return next.replace(/\n{3,}/g, "\n\n");
}

function insertProgressiveContext(systemPrompt: string, progressiveContext: string): string {
	const insertion = `\n\n${progressiveContext}\n`;
	const skillMarker = "\n\nThe following skills provide specialized instructions";
	const dateMarker = "\nCurrent date:";
	if (systemPrompt.includes(skillMarker)) return systemPrompt.replace(skillMarker, `${insertion}${skillMarker}`);
	if (systemPrompt.includes(dateMarker)) return systemPrompt.replace(dateMarker, `${insertion}${dateMarker}`);
	return `${systemPrompt}${insertion}`;
}

function progressiveSystemPrompt(options: BuildSystemPromptOptions, systemPrompt: string, prompt: string): string {
	const contextFiles = options.contextFiles ?? [];
	if (contextFiles.length === 0) return systemPrompt;
	latestContexts = contextFiles.map(parseContext);
	const compact = buildProgressiveContext(latestContexts, prompt);
	const withoutOriginal = removeOriginalContext(systemPrompt, contextFiles);
	return insertProgressiveContext(withoutOriginal, compact);
}

function loadContext(path: string, mode: "summary" | "full" | "manifest"): string {
	if (mode === "manifest") return buildManifest(latestContexts, "");
	const exact = latestContexts.find((ctx) => ctx.path === path || ctx.path.endsWith(path));
	if (!exact) {
		return `Context not found: ${path}\n\nAvailable:\n${latestContexts.map((ctx) => `- ${ctx.path}`).join("\n")}`;
	}
	if (mode === "summary") return `# ${exact.path}\n\n${exact.summary}`;
	return `# ${exact.path}\n\n${exact.body}`;
}

export default function progressiveContext(pi: ExtensionAPI) {
	pi.registerTool({
		name: "context_load",
		label: "context_load",
		description: "Load project context progressively. Use for AGENTS.md/project rules when manifest says context likely relevant.",
		parameters: {
			type: "object",
			properties: {
				path: { type: "string", description: "Context path from available_context, or suffix like modules/darwin/AGENTS.md" },
				mode: { type: "string", enum: ["summary", "full", "manifest"], description: "summary, full, or manifest" },
			},
			required: ["path"],
			additionalProperties: false,
		} as any,
		async execute(_toolCallId, params: { path: string; mode?: "summary" | "full" | "manifest" }) {
			return {
				content: [{ type: "text", text: loadContext(params.path, params.mode ?? "full") }],
				details: {},
			};
		},
	});

	pi.registerCommand("context-manifest", {
		description: "Show progressive context manifest",
		handler: async (_args, ctx) => {
			ctx.ui.notify(buildManifest(latestContexts, ""), "info");
		},
	});

	pi.on("before_agent_start", async (event) => {
		return {
			systemPrompt: progressiveSystemPrompt(event.systemPromptOptions, event.systemPrompt, event.prompt),
		};
	});

	pi.on("session_before_compact", async (_event, ctx) => {
		if (latestContexts.length > 0) {
			ctx.ui.notify("Progressive context active: compaction keeps manifest/summaries, full context remains loadable via context_load.", "info");
		}
	});
}
