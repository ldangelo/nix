/**
 * Pi Notify Extension
 *
 * Sends a native terminal notification when Pi agent is done and waiting for input.
 * Supports multiple terminal protocols:
 * - OSC 777: Ghostty, iTerm2, WezTerm, rxvt-unicode
 * - OSC 99: Kitty
 * - Windows toast: Windows Terminal (WSL)
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";

function windowsToastScript(title: string, body: string): string {
	const type = "Windows.UI.Notifications";
	const mgr = `[${type}.ToastNotificationManager, ${type}, ContentType = WindowsRuntime]`;
	const template = `[${type}.ToastTemplateType]::ToastText01`;
	const toast = `[${type}.ToastNotification]::new($xml)`;
	return [
		`${mgr} > $null`,
		`$xml = [${type}.ToastNotificationManager]::GetTemplateContent(${template})`,
		`$xml.GetElementsByTagName('text')[0].AppendChild($xml.CreateTextNode('${body}')) > $null`,
		`[${type}.ToastNotificationManager]::CreateToastNotifier('${title}').Show(${toast})`,
	].join("; ");
}

function notifyOSC777(title: string, body: string): void {
	process.stdout.write(`\x1b]777;notify;${title};${body}\x07`);
}

function notifyOSC99(title: string, body: string): void {
	// Kitty OSC 99: i=notification id, d=0 means not done yet, p=body for second part
	process.stdout.write(`\x1b]99;i=1:d=0;${title}\x1b\\`);
	process.stdout.write(`\x1b]99;i=1:p=body;${body}\x1b\\`);
}

function notifyWindows(title: string, body: string): void {
	const { execFile } = require("child_process");
	execFile("powershell.exe", ["-NoProfile", "-Command", windowsToastScript(title, body)]);
}

function bell(): void {
	// Raw BEL. tmux catches this via alert-bell and emits session/window-aware macOS notification.
	process.stdout.write("\x07");
}

function notify(title: string, body: string): void {
	bell();
	if (process.env.WT_SESSION) {
		notifyWindows(title, body);
	} else if (process.env.KITTY_WINDOW_ID) {
		notifyOSC99(title, body);
	} else {
		notifyOSC777(title, body);
	}
}

export default function (pi: ExtensionAPI) {
	pi.registerTool({
		name: "notify_user",
		label: "Notify User",
		description: "Send a terminal/tmux notification to the user when attention is needed.",
		promptSnippet: "Send a terminal/tmux notification to the user when attention is needed.",
		promptGuidelines: [
			"Use notify_user when work is complete, blocked, or waiting on user attention in a long-running/async workflow.",
			"Do not spam notify_user; call it at meaningful completion, block, or handoff points only.",
		],
		parameters: Type.Object({
			title: Type.Optional(Type.String({ description: "Notification title. Default: Pi" })),
			body: Type.Optional(Type.String({ description: "Notification body. Default: Ready for input" })),
		}),
		async execute(_toolCallId, params) {
			const title = params.title?.trim() || "Pi";
			const body = params.body?.trim() || "Ready for input";
			notify(title, body);
			return {
				content: [{ type: "text", text: `Notified user: ${title} — ${body}` }],
				details: { title, body },
			};
		},
	});

	pi.registerCommand("notify", {
		description: "Send a tmux/terminal notification now",
		handler: async (args) => {
			notify("Pi", args?.trim() || "Ready for input");
		},
	});

	pi.on("agent_end", async () => {
		notify("Pi", "Ready for input");
	});
}
