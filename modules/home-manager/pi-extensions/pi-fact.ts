/**
 * /pi-fact — append an always-visible durable fact to Pi memory in Obsidian.
 *
 * Uses same Markdown store as pi-memory via PI_MEMORY_DIR. The memory file lives
 * in the Obsidian vault, so user can inspect/edit what Pi treats as true.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { appendFileSync, existsSync, mkdirSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";

const DEFAULT_VAULT = "/Users/ldangelo/Library/Mobile Documents/iCloud~md~obsidian/Documents/ldangelo";
const DEFAULT_MEMORY_DIR = `${DEFAULT_VAULT}/Agent/PiMemory`;

function memoryDir(): string {
	return process.env.PI_MEMORY_DIR || DEFAULT_MEMORY_DIR || join(homedir(), ".pi", "agent", "memory");
}

function likelySecret(text: string): boolean {
	return /(?:sk-[a-zA-Z0-9_-]{20,}|AKIA[0-9A-Z]{16}|-----BEGIN [A-Z ]*PRIVATE KEY-----|ghp_[A-Za-z0-9_]{30,}|xox[baprs]-[A-Za-z0-9-]{20,})/.test(text);
}

function sanitizeFact(raw: string): string {
	return raw.trim().replace(/\s+/g, " ");
}

function idFor(date: Date): string {
	return `${date.toISOString().replace(/[-:.TZ]/g, "").slice(0, 14)}-${Math.random().toString(36).slice(2, 8)}`;
}

export default function (pi: ExtensionAPI) {
	pi.registerCommand("pi-fact", {
		description: "Persist a durable fact to Pi memory in Obsidian (usage: /pi-fact <fact>)",
		handler: async (args, ctx) => {
			let fact = sanitizeFact(args);
			if (!fact) {
				if (!ctx.hasUI) {
					ctx.ui.notify("Usage: /pi-fact <fact>", "error");
					return;
				}
				const entered = await ctx.ui.input("Fact for Pi to always remember:", "");
				fact = sanitizeFact(entered || "");
			}

			if (!fact) {
				ctx.ui.notify("No fact saved.", "warning");
				return;
			}
			if (likelySecret(fact)) {
				ctx.ui.notify("Refusing to save likely secret/API key.", "error");
				return;
			}

			const dir = memoryDir();
			if (!existsSync(dir)) mkdirSync(dir, { recursive: true });

			const now = new Date();
			const entry = `\n<!-- ${now.toISOString()} [${idFor(now)}] -->\n#fact [[pi-fact]] ${fact}\n`;
			const file = join(dir, "MEMORY.md");
			appendFileSync(file, entry, "utf8");

			pi.sendMessage({
				customType: "pi-fact",
				content: `Durable Pi fact saved to Obsidian memory. Treat as user-confirmed context unless workspace evidence contradicts it.\n\nFact: ${fact}\nPath: ${file}`,
				display: true,
				details: { file, fact, timestamp: now.toISOString() },
			});

			ctx.ui.notify(`Saved Pi fact → ${file}`, "info");
		},
	});
}
