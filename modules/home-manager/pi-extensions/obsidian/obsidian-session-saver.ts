/**
 * Obsidian Session Saver Extension
 *
 * Saves a readable Pi session log into Obsidian at normal quit. The log is
 * intentionally plain Markdown so it can be inspected, searched, and corrected.
 */

import type { ExtensionAPI, SessionEntry } from "@earendil-works/pi-coding-agent";
import { existsSync, mkdirSync, writeFileSync } from "node:fs";
import { basename } from "node:path";

const VAULT = process.env.PI_OBSIDIAN_VAULT || "/Users/ldangelo/Library/Mobile Documents/iCloud~md~obsidian/Documents/ldangelo";
const MAX_TRANSCRIPT_CHARS = 30_000;
const MAX_MESSAGE_CHARS = 4_000;

function textFromContent(content: unknown): string {
	if (typeof content === "string") return content;
	if (!Array.isArray(content)) return "";
	return content
		.filter((c): c is { type: string; text: string } =>
			typeof c === "object" && c !== null && (c as { type?: unknown }).type === "text" && typeof (c as { text?: unknown }).text === "string",
		)
		.map((c) => c.text)
		.join("\n");
}

function truncate(text: string, max: number): string {
	return text.length > max ? `${text.slice(0, max)}\n...[truncated ${text.length - max} chars]` : text;
}

function entryLine(entry: SessionEntry): string | undefined {
	if (entry.type !== "message") return undefined;
	const role = entry.message.role;
	if (role !== "user" && role !== "assistant") return undefined;
	const text = truncate(textFromContent(entry.message.content).trim(), MAX_MESSAGE_CHARS);
	if (!text) return undefined;
	return `### ${role}\n\n${text}`;
}

function collectTranscript(entries: SessionEntry[]): string {
	const lines = entries.map(entryLine).filter((line): line is string => Boolean(line));
	return truncate(lines.join("\n\n---\n\n"), MAX_TRANSCRIPT_CHARS);
}

function countToolResults(entries: SessionEntry[]): number {
	return entries.filter((entry) => entry.type === "message" && entry.message.role === "toolResult").length;
}

function inferProject(sessionPath: string, cwd: string | undefined): string {
	const match = sessionPath.match(/sessions\/--(.+?)--\//i);
	if (match) return match[1].replace(/-/g, "/");
	return cwd ? basename(cwd) : "unknown";
}

export default function (pi: ExtensionAPI) {
	pi.on("session_shutdown", async (event, ctx) => {
		if (event.reason !== "quit") return;

		const sessionsDir = `${VAULT}/Sessions/Pi`;
		if (!existsSync(sessionsDir)) mkdirSync(sessionsDir, { recursive: true });

		const sessionFile = ctx.sessionManager.getSessionFile();
		if (!sessionFile) return;

		const sessionId = basename(sessionFile).replace(/\.jsonl$/, "");
		const entries = ctx.sessionManager.getEntries();
		const header = ctx.sessionManager.getHeader();
		const sessionDate = header?.timestamp?.split("T")[0] || new Date().toISOString().split("T")[0];
		const projectName = inferProject(sessionFile, ctx.cwd);
		const toolResultCount = countToolResults(entries);
		const transcript = collectTranscript(entries);

		const markdownContent = `---
date: ${sessionDate}
project: ${projectName}
tags: [pi-session, auto-saved]
source: pi
session_id: ${sessionId}
---

# Pi Session — ${sessionDate}

## Summary
Auto-saved Pi session log. Review/correct this note if needed; it is for human-visible memory, not hidden truth.

## Project
${projectName}

## Transcript excerpt
${transcript || "No user/assistant transcript captured."}

## Statistics
- Tool results: ${toolResultCount}
- Total entries: ${entries.length}
- Reason: ${event.reason}
- Session file: ${sessionFile}

## Durable facts to promote
Use \`/pi-fact <fact>\` for any facts from this session Pi should always see.

## Open items
- [ ] Review session log.
`;

		const outputPath = `${sessionsDir}/${sessionId}.md`;
		writeFileSync(outputPath, markdownContent, "utf8");

		if (ctx.hasUI) ctx.ui.notify(`Session log saved to: ${outputPath}`, "info");
	});
}
