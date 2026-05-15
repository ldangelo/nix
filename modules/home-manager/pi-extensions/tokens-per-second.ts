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

import type { ExtensionAPI, MessageUpdateEvent, TurnEndEvent, TurnStartEvent } from "@earendil-works/pi-coding-agent";

export default function (pi: ExtensionAPI) {
	let streamStartTime: number | null = null;
	let tokenCount: number = 0;
	let currentTps: number = 0;
	let turnIndex: number = 0;

	const updateStatus = (ctx: ExtensionAPI, tps: number, label: string) => {
		const theme = ctx.ui.theme;
		const tpsStr = tps.toFixed(1);
		const color = tps >= 50 ? "success" : tps >= 20 ? "accent" : "dim";
		ctx.ui.setStatus("tps", theme.fg(color, `${label} ${tpsStr} t/s`));
	};

	const clearStatus = (ctx: ExtensionAPI) => {
		ctx.ui.setStatus("tps", undefined);
	};

	pi.on("turn_start", async (event: TurnStartEvent, ctx) => {
		turnIndex = event.turnIndex;
		streamStartTime = null;
		tokenCount = 0;
		currentTps = 0;
	});

	pi.on("message_update", async (event: MessageUpdateEvent, ctx) => {
		const { assistantMessageEvent } = event;

		// Count text tokens (approximate by character count / 4)
		if (assistantMessageEvent.type === "text_delta") {
			if (streamStartTime === null) {
				streamStartTime = Date.now();
			}
			tokenCount += assistantMessageEvent.delta.length / 4;

			// Calculate current tps
			const elapsed = (Date.now() - streamStartTime) / 1000;
			if (elapsed > 0) {
				currentTps = tokenCount / elapsed;
				updateStatus(ctx, currentTps, "●");
			}
		}

		// Also count thinking tokens
		if (assistantMessageEvent.type === "thinking_delta") {
			if (streamStartTime === null) {
				streamStartTime = Date.now();
			}
			tokenCount += assistantMessageEvent.thinking.length / 4;

			const elapsed = (Date.now() - streamStartTime) / 1000;
			if (elapsed > 0) {
				currentTps = tokenCount / elapsed;
				updateStatus(ctx, currentTps, "●");
			}
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