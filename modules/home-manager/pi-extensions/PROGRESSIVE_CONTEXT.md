# Progressive Context Extension

`progressive-context.ts` replaces full inline project context with compact progressive disclosure at each agent turn.

## What it does

- Removes full `AGENTS.md` blocks from active system prompt.
- Inserts `<available_context>` manifest with path, disclosure mode, token estimate, triggers.
- Keeps small files inline; summarizes larger files by default.
- Supports frontmatter controls:

```md
---
disclose: always   # always | summary | lazy | disabled
summary: Short description shown in manifest
triggers: [nix, darwin, deploy]
---
```

- Adds `context_load` tool for full or summary context loading.
- Adds `/context-manifest` command.
- Adds compaction notice: full context remains loadable after compaction.

## Disclosure modes

- `always`: full body inline
- `summary`: summary + headings + critical excerpts inline
- `lazy`: manifest only; load with `context_load` or `read`
- `disabled`: omit from manifest and inline blocks

Default: small files `always`, larger files `summary`.
