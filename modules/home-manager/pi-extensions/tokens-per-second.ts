/**
 * Tokens Per Second Extension
 *
 * Tracks streaming speed and displays tokens/second in the status line.
 * Shows current throughput during LLM responses and final average at turn end.
 *
 * Usage:
 *   - Extension auto-enables on load
 *   - During streaming: shows live t/s in status line
 *   - At turn end: shows final average t/s briefly before clearing
 */

import type {
	ExtensionAPI,
	ExtensionContext,
	MessageUpdateEvent,
	TurnEndEvent,
	TurnStartEvent,
} from "@earendil-works/pi-coding-agent";

type AssistantDeltaEvent = MessageUpdateEvent["assistantMessageEvent"] & {
	delta?: unknown;
	thinking?: unknown;
};

export default function (pi: ExtensionAPI) {
	let streamStartTime: number | null = null;
	let tokenCount: number = 0;
	let currentTps: number = 0;

	const updateStatus = (ctx: ExtensionContext, tps: number, label: string) => {
		const theme = ctx.ui.theme;
		const tpsStr = tps.toFixed(1);
		const color = tps >= 50 ? "success" : tps >= 20 ? "accent" : "dim";
		ctx.ui.setStatus("tps", theme.fg(color, `${label} ${tpsStr} t/s`));
	};

	const clearStatus = (ctx: ExtensionContext) => {
		ctx.ui.setStatus("tps", undefined);
	};

	const getDeltaText = (assistantMessageEvent: AssistantDeltaEvent): string => {
		if (typeof assistantMessageEvent.delta === "string") {
			return assistantMessageEvent.delta;
		}

		// Backwards compatibility for older pi-ai thinking_delta payloads.
		if (typeof assistantMessageEvent.thinking === "string") {
			return assistantMessageEvent.thinking;
		}

		return "";
	};

	const recordDelta = (ctx: ExtensionContext, delta: string) => {
		if (delta.length === 0) return;

		if (streamStartTime === null) {
			streamStartTime = Date.now();
		}
		tokenCount += delta.length / 4;

		const elapsed = (Date.now() - streamStartTime) / 1000;
		if (elapsed > 0) {
			currentTps = tokenCount / elapsed;
			updateStatus(ctx, currentTps, "●");
		}
	};

	pi.on("turn_start", async (_event: TurnStartEvent, _ctx) => {
		streamStartTime = null;
		tokenCount = 0;
		currentTps = 0;
	});

	pi.on("message_update", async (event: MessageUpdateEvent, ctx) => {
		const { assistantMessageEvent } = event;

		// Count streamed text/thinking tokens (approximate by character count / 4).
		if (assistantMessageEvent.type === "text_delta" || assistantMessageEvent.type === "thinking_delta") {
			recordDelta(ctx, getDeltaText(assistantMessageEvent));
		}
	});

	pi.on("turn_end", async (event: TurnEndEvent, ctx) => {
		if (streamStartTime !== null && tokenCount > 0) {
			const elapsed = (Date.now() - streamStartTime) / 1000;
			const avgTps = tokenCount / elapsed;

			// Show final average for 3 seconds then clear
			updateStatus(ctx, avgTps, "✓");
			setTimeout(() => clearStatus(ctx), 3000);
		} else {
			clearStatus(ctx);
		}

		streamStartTime = null;
		tokenCount = 0;
		currentTps = 0;
	});
}