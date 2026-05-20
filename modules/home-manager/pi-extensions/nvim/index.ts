/**
 * Neovim Extension for Pi
 *
 * Opens neovim with full terminal control, suspending Pi's TUI while editing.
 *
 * Features:
 *   /nvim [file[:line]]   - Command to open nvim from the editor
 *   open_in_nvim           - Tool the LLM can call to let the user edit a file
 *   Ctrl+N                - Shortcut to open nvim (no file)
 *
 * Examples:
 *   /nvim                        # Open nvim in cwd
 *   /nvim src/index.ts           # Open a specific file
 *   /nvim src/index.ts:42        # Open at a specific line
 *   /nvim src/a.ts src/b.ts      # Open multiple files
 *
 * The LLM can also call the open_in_nvim tool to let you edit files interactively.
 */

import { spawnSync } from "node:child_process";
import { existsSync } from "node:fs";
import { resolve } from "node:path";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { Type } from "@sinclair/typebox";

/** Find the nvim binary, falling back to vim/vi */
function findEditor(): string {
  for (const editor of ["nvim", "vim", "vi"]) {
    const which = spawnSync("which", [editor], { encoding: "utf-8" });
    if (which.status === 0 && which.stdout.trim()) {
      return which.stdout.trim();
    }
  }
  return "vi"; // last resort
}

/** Parse "file:line" into [file, line?] */
function parseFileArg(arg: string): { file: string; line?: number } {
  // Match file:number at the end
  const match = arg.match(/^(.+):(\d+)$/);
  if (match) {
    return { file: match[1], line: parseInt(match[2], 10) };
  }
  return { file: arg };
}

/** Build nvim args from file specs */
function buildNvimArgs(
  editorPath: string,
  files: { file: string; line?: number }[],
  cwd: string,
): string[] {
  const args: string[] = [];

  if (files.length === 0) {
    return args;
  }

  // For the first file with a line number, use +N
  const first = files[0];
  if (first.line) {
    args.push(`+${first.line}`);
  }

  // Add all file paths (resolved against cwd)
  for (const f of files) {
    const resolved = resolve(cwd, f.file);
    args.push(resolved);
  }

  return args;
}

/** Launch nvim, suspending Pi's TUI. Returns exit code. */
function launchNvim(
  tui: { stop: () => void; start: () => void; requestRender: (force?: boolean) => void },
  editorPath: string,
  args: string[],
): number | null {
  // Suspend Pi's TUI to release the terminal
  tui.stop();

  // Switch to alternate screen and clear
  process.stdout.write("\x1b[?1049h\x1b[2J\x1b[H");

  // Spawn nvim with full terminal access
  const result = spawnSync(editorPath, args, {
    stdio: "inherit",
    env: { ...process.env, TERM: process.env.TERM || "xterm-256color" },
  });

  // Leave alternate screen
  process.stdout.write("\x1b[?1049l");

  // Resume Pi's TUI
  tui.start();
  tui.requestRender(true);

  return result.status;
}

export default function (pi: ExtensionAPI) {
  const editorPath = findEditor();
  const isNvim = editorPath.includes("nvim");
  const editorName = isNvim ? "nvim" : editorPath.split("/").pop() || "vi";

  // ── /nvim command ──────────────────────────────────────────────────
  pi.registerCommand("nvim", {
    description: `Open ${editorName} with full terminal control`,

    handler: async (args, ctx) => {
      if (!ctx.hasUI) {
        ctx.ui.notify("nvim requires interactive mode", "error");
        return;
      }

      // Parse arguments: /nvim file1:line file2 ...
      const fileSpecs = args
        ? args
          .split(/\s+/)
          .filter(Boolean)
          .map(parseFileArg)
        : [];

      // Validate files exist (warn but don't block - nvim can create new files)
      for (const spec of fileSpecs) {
        const resolved = resolve(ctx.cwd, spec.file);
        if (!existsSync(resolved)) {
          ctx.ui.notify(`New file: ${spec.file}`, "info");
        }
      }

      const nvimArgs = buildNvimArgs(editorPath, fileSpecs, ctx.cwd);

      await ctx.ui.custom<number | null>((tui, _theme, _kb, done) => {
        const exitCode = launchNvim(tui, editorPath, nvimArgs);
        done(exitCode);
        return { render: () => [], invalidate: () => { } };
      });
    },
  });

  // ── open_in_nvim tool (LLM-callable) ───────────────────────────────
  pi.registerTool({
    name: "open_in_nvim",
    label: "Open in Neovim",
    description:
      "Open a file in the user's neovim editor for interactive editing. " +
      "Use this when the user wants to manually edit a file, review changes interactively, " +
      "or when the task benefits from the user's direct input in their editor. " +
      "Pi's TUI is suspended while the editor is open.",
    promptSnippet: "Open files in the user's neovim for interactive editing",
    promptGuidelines: [
      "Use open_in_nvim when the user explicitly asks to edit a file in their editor, " +
      "or when interactive editing is more appropriate than automated changes.",
    ],
    parameters: Type.Object({
      file: Type.String({ description: "File path to open (relative to project root)" }),
      line: Type.Optional(
        Type.Number({ description: "Line number to jump to" }),
      ),
      reason: Type.Optional(
        Type.String({
          description: "Brief explanation of why the user should edit this file",
        }),
      ),
    }),

    async execute(toolCallId, params, signal, onUpdate, ctx) {
      if (!ctx.hasUI) {
        throw new Error("open_in_nvim requires interactive mode (not available in print/RPC mode)");
      }

      const resolved = resolve(ctx.cwd, params.file);
      const fileExists = existsSync(resolved);

      // Notify the user why we're opening the editor
      if (params.reason) {
        ctx.ui.notify(params.reason, "info");
      }

      const fileSpecs = [{ file: params.file, line: params.line }];
      const nvimArgs = buildNvimArgs(editorPath, fileSpecs, ctx.cwd);

      const exitCode = await ctx.ui.custom<number | null>((tui, _theme, _kb, done) => {
        const code = launchNvim(tui, editorPath, nvimArgs);
        done(code);
        return { render: () => [], invalidate: () => { } };
      });

      const success = exitCode === 0;
      const status = success
        ? `User finished editing ${params.file} in ${editorName}`
        : `${editorName} exited with code ${exitCode}`;

      return {
        content: [{ type: "text", text: status }],
        details: {
          file: params.file,
          line: params.line,
          exitCode,
          editorUsed: editorName,
          fileExisted: fileExists,
        },
      };
    },
  });

  // ── Ctrl+N shortcut ────────────────────────────────────────────────
  pi.registerShortcut("ctrl+shift+e", {
    description: `Open ${editorName}`,
    handler: async (ctx) => {
      if (!ctx.hasUI) return;

      await ctx.ui.custom<number | null>((tui, _theme, _kb, done) => {
        const code = launchNvim(tui, editorPath, []);
        done(code);
        return { render: () => [], invalidate: () => { } };
      });
    },
  });

  // ── Startup notification ───────────────────────────────────────────
  pi.on("session_start", async (_event, ctx) => {
    if (!ctx.hasUI) return;
    ctx.ui.setStatus("nvim", `${editorName} ready (Ctrl+N or /nvim)`);
    // Clear status after 3 seconds. Context may be stale after print-mode session teardown/reload.
    setTimeout(() => {
      try {
        if (ctx.hasUI) ctx.ui.setStatus("nvim", undefined);
      } catch {
        // Ignore stale extension context after session replacement/reload.
      }
    }, 3000);
  });
}
