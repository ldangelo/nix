import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

const DEFAULT_THRESHOLD_PERCENT = 60;
const MIN_THRESHOLD_PERCENT = 10;
const MAX_THRESHOLD_PERCENT = 95;

function parseThreshold(): number {
	const raw = process.env.PI_AUTO_COMPACT_THRESHOLD_PERCENT;
	if (!raw) return DEFAULT_THRESHOLD_PERCENT;
	const parsed = Number(raw);
	if (!Number.isFinite(parsed)) return DEFAULT_THRESHOLD_PERCENT;
	return Math.min(MAX_THRESHOLD_PERCENT, Math.max(MIN_THRESHOLD_PERCENT, parsed));
}

function formatUsage(ctx: ExtensionContext): string {
	const usage = ctx.getContextUsage();
	if (!usage || usage.percent == null) return "context usage unknown";
	const tokens = usage.tokens == null ? "?" : Math.round(usage.tokens).toLocaleString();
	const window = Math.round(usage.contextWindow).toLocaleString();
	return `${usage.percent.toFixed(1)}% (${tokens}/${window} tokens)`;
}

export default function autoCompact(pi: ExtensionAPI) {
	const threshold = parseThreshold();
	let compacting = false;
	let lastTriggeredAtTokens: number | null = null;

	pi.on("agent_end", (_event, ctx) => {
		if (compacting) return;

		const usage = ctx.getContextUsage();
		if (!usage || usage.percent == null || usage.tokens == null) return;
		if (usage.percent < threshold) return;

		// Avoid retriggering on the same usage estimate if a compaction failed or is still settling.
		if (lastTriggeredAtTokens !== null && Math.abs(usage.tokens - lastTriggeredAtTokens) < 512) return;

		compacting = true;
		lastTriggeredAtTokens = usage.tokens;
		ctx.ui.setStatus("auto-compact", `compact:${usage.percent.toFixed(0)}%`);
		ctx.ui.notify(`Auto-compacting at ${formatUsage(ctx)} (threshold ${threshold}%)`, "info");

		ctx.compact({
			customInstructions:
				"Preserve current goal, user constraints/preferences, key decisions, files read/modified, validation status, blockers, and exact next steps. Keep paths and commands exact. Note durable facts that should be promoted to memory.",
			onComplete: () => {
				compacting = false;
				ctx.ui.setStatus("auto-compact", undefined);
			},
			onError: (error) => {
				compacting = false;
				ctx.ui.setStatus("auto-compact", undefined);
				ctx.ui.notify(`Auto-compaction failed: ${error.message}`, "warning");
			},
		});
	});

	pi.on("session_compact", () => {
		compacting = false;
		lastTriggeredAtTokens = null;
	});

	pi.on("session_start", () => {
		compacting = false;
		lastTriggeredAtTokens = null;
	});
}
