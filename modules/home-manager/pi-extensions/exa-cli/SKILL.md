---
name: exa-cli
description: Lightweight Exa semantic web search and URL fetch via pi-exa CLI. Use for broad web discovery, current facts, finding sources, or reading known URLs without loading Exa MCP schemas.
---

# Exa CLI

Use bash CLI. Do not call Exa MCP tools directly unless CLI fails.

## Commands

```bash
pi-exa search 'semantic query describing ideal page' --num 10
pi-exa fetch https://example.com/article --max 6000
pi-exa fetch https://a.example https://b.example --max 3000
```

## Rules

- Search query: describe ideal page, not keyword soup.
- Need facts/sources/current info → `search` first.
- Highlights insufficient → `fetch` best URLs.
- Broad research → 2-4 varied searches, then fetch top sources.
- Keep `--num` small unless user asks broad survey.
