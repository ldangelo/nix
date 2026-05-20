---
name: sequentialthinking-cli
description: Lightweight Sequential Thinking via pi-think CLI. Use for complex planning, debugging, design tradeoffs, or multi-step reasoning without loading Sequential Thinking MCP schema.
---

# Sequential Thinking CLI

Use bash CLI. Do not call sequentialthinking MCP tool directly unless CLI fails.

## Commands

```bash
pi-think 'Break problem into likely causes' --n 1 --total 4
pi-think 'Hypothesis: cache invalidation bug; verify with logs' --n 2 --total 4
pi-think 'Conclusion: fix stale key and add test' --n 4 --total 4 --done

pi-think --json '{"thought":"revise prior plan","thoughtNumber":3,"totalThoughts":5,"nextThoughtNeeded":true,"isRevision":true,"revisesThought":2}'
```

## Rules

- Use for high-ambiguity or multi-step work only.
- Keep thoughts short. Store decisions in final answer or issue notes, not in endless thoughts.
- Use `--json` for revision/branch fields.
