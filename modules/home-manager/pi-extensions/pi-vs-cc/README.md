# pi-vs-cc vendored extensions

Source: https://github.com/disler/pi-vs-claude-code

Vendored subset only:

- `agent-chain`
- `agent-team`
- `system-select`
- `tilldone`
- `coms`
- `coms-net`
- `scripts/coms-net-server.ts`
- sample `.pi/agents` definitions

Local patches:

- `@mariozechner/pi-coding-agent` → `@earendil-works/pi-coding-agent`
- `@mariozechner/pi-tui` → `@earendil-works/pi-tui`
- `@mariozechner/pi-ai` → `@earendil-works/pi-ai`
- `@sinclair/typebox` → `typebox`

Do not auto-load these globally. Use Home Manager wrapper scripts (`pi-chain`, `pi-team`, `pi-system`, `pi-tilldone`, `pi-coms`, `pi-coms-net`).
