/**
 * Obsidian Session Saver Extension
 *
 * Automatically saves session logs to the Obsidian vault at session end.
 * This enables persistent context across pi agent sessions.
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { existsSync, mkdirSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";

// Obsidian vault path - can be overridden via environment variable
const VAULT = process.env.PI_OBSIDIAN_VAULT || "/Users/ldangelo/Library/Mobile Documents/iCloud~md~obsidian/Documents/ldangelo";

export default function (pi: ExtensionAPI) {
	pi.on("session_shutdown", async (event, ctx) => {
		// Only save on actual shutdown (quit), not on reload/new/resume
		if (event.reason !== "quit") {
			return;
		}

		// Ensure vault and sessions directory exist
		const sessionsDir = `${VAULT}/Sessions`;
		if (!existsSync(sessionsDir)) {
			mkdirSync(sessionsDir, { recursive: true });
		}

		// Get session file path and ID
		const sessionFile = ctx.sessionManager.getSessionFile();
		if (!sessionFile) {
			return;
		}

		const sessionPath = sessionFile;
		const sessionFileName = sessionFile.split("/").pop() || "session";
		const sessionId = sessionFileName.replace(/\.jsonl$/, "");

		// Parse session to extract key information
		const entries = ctx.sessionManager.getEntries();
		let lastUserContent = "";
		let lastAssistantContent = "";
		let toolCallsCount = 0;

		for (const entry of entries) {
			if (entry.type === "message") {
				if (entry.message.role === "user" && Array.isArray(entry.message.content)) {
					lastUserContent = entry.message.content
						.filter((c): c is { type: "text"; text: string } => c.type === "text")
						.map((c) => c.text)
						.join("\n");
				} else if (entry.message.role === "assistant") {
					lastAssistantContent = entry.message.content
						.filter((c): c is { type: "text"; text: string } => c.type === "text")
						.map((c) => c.text)
						.join("\n");
				}
			}
			if (entry.type === "toolResult") {
				toolCallsCount++;
			}
		}

		// Get session metadata from the header
		const header = ctx.sessionManager.getHeader();
		const sessionDate = header.timestamp.split("T")[0] || new Date().toISOString().split("T")[0];

		// Determine project name from session path
		const projectMatch = sessionPath.match(/Sessions\/--([^--]+)--/);
		const projectName = projectMatch ? projectMatch[1].replace(/-/g, "/") : "unknown";

		// Generate markdown content for session log
		const markdownContent = `---
date: ${sessionDate}
project: ${projectName}
tags: [pi-session, auto-saved]
---

# Session — ${sessionDate}

## Summary
This session log was automatically saved by the pi agent's Obsidian integration.

## Conversation Highlights
${lastUserContent.slice(0, 2000)}${lastUserContent.length > 2000 ? "..." : ""}

## Key Results
${lastAssistantContent.slice(0, 2000)}${lastAssistantContent.length > 2000 ? "..." : ""}

## Statistics
- Tool calls: ${toolCallsCount}
- Total entries: ${entries.length}
- Reason: ${event.reason}

## Open Items
*Review the session log and add any pending tasks here.*

## Files Modified
*Review the session log and list any files modified here.*

## Key Learnings
*Review the session log and note any important learnings here.*
`;

		// Write session log to vault
		const outputPath = `${sessionsDir}/${sessionId}.md`;
		writeFileSync(outputPath, markdownContent, "utf8");

		if (ctx.hasUI) {
			ctx.ui.notify(`Session log saved to: ${outputPath}`, "info");
		}
	});
}
