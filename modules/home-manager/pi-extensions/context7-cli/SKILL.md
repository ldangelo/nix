---
name: context7-cli
description: Lightweight Context7 library documentation lookup via pi-context7 CLI. Use when user asks for up-to-date library/framework/package docs, APIs, examples, or version-specific usage.
---

# Context7 CLI

Use bash CLI. Do not call Context7 MCP tools directly unless CLI fails.

## Commands

```bash
pi-context7 resolve react
pi-context7 resolve 'next.js app router'
pi-context7 docs /vercel/next.js --topic routing --tokens 8000
pi-context7 docs /reactjs/react.dev --topic useEffect --tokens 5000
```

## Workflow

1. If user did not provide `/org/project` library ID, run `pi-context7 resolve <name>`.
2. Pick best match by exact name, docs coverage, trust score.
3. Run `pi-context7 docs <id> --topic <topic>`.

## Rules

- User-provided `/org/project` or `/org/project/version` → skip resolve.
- Keep `--tokens` focused; raise only if docs insufficient.
