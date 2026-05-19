# Pi vs Claude Code Extensions

Vendored opt-in extensions from `disler/pi-vs-claude-code` live under:

```text
~/.pi/agent/vendor/pi-vs-cc/
```

They are **not auto-loaded**. Use wrapper commands so normal `pi` sessions stay stable.

## Commands

| Command | Extension | Use when |
| --- | --- | --- |
| `pi-chain` | `agent-chain` | Sequential workflows: plan → build → review |
| `pi-team` | `agent-team` | Dispatcher delegates to specialist agents |
| `pi-system` | `system-select` | Pick persona/system prompt via `/system` |
| `pi-tilldone` | `tilldone` | Force task definition/progress before tool use |
| `pi-coms` | `coms` | Same-machine peer Pi agents talk directly |
| `pi-coms-net` | `coms-net` | Pi agents talk through HTTP/SSE hub |
| `pi-coms-net-server` | server | Start `coms-net` hub |
| `pi-vs-cc-agents-init` | helper | Copy sample agents/chains/teams into project |

## One-time project setup for chain/team

`agent-chain` and `agent-team` read project-local config from `.pi/agents/`.

```bash
cd /path/to/project
pi-vs-cc-agents-init
```

Creates:

```text
.pi/agents/
  agent-chain.yaml
  teams.yaml
  planner.md
  builder.md
  reviewer.md
  scout.md
  red-team.md
  documenter.md
  bowser.md
  plan-reviewer.md
```

Edit those files per project.

## `pi-chain`

Start:

```bash
pi-chain
```

Inside Pi:

```text
/chain       # select active chain
/chain-list  # list chains
```

Best default workflow: ask for plan/build/review style work. Extension runs chain steps and stores subagent sessions in `.pi/agent-sessions/`.

## `pi-team`

Start:

```bash
pi-team
```

Inside Pi:

```text
/agents-team     # switch team
/agents-list     # list agents
/agents-grid 3   # set dashboard columns
```

Use when you want dispatcher-style delegation to named specialists from `.pi/agents/teams.yaml`.

## `pi-system`

Start:

```bash
pi-system
```

Inside Pi:

```text
/system
```

Scans:

```text
./.pi/agents/
./.claude/agents/
./.gemini/agents/
./.codex/agents/
~/.pi/agent/agents/
~/.claude/agents/
```

Use for manual persona switching without launching chain/team orchestration.

## `pi-tilldone`

Start:

```bash
pi-tilldone
```

Inside Pi:

```text
/tilldone
```

Agent must create/update tasks before normal work. Use sparingly: this overlaps Beads and local `todo` extension.

## `pi-coms` same-machine peer agents

Terminal 1:

```bash
pi-coms --name planner --purpose "Plans work" --color "#36F9F6"
```

Terminal 2:

```bash
pi-coms --name builder --purpose "Edits code" --color "#72F1B8"
```

Tools exposed to agents:

```text
coms_list
coms_send
coms_get
coms_await
```

Useful prompt:

```text
List peers, ask planner for implementation risks, wait for reply, then continue.
```

Local registry/socket data lives under `~/.pi/coms/`.

## `pi-coms-net` networked peer agents

Localhost hub:

```bash
pi-coms-net-server
pi-coms-net --name dev
pi-coms-net --name reviewer
```

LAN/remote hub:

```bash
export PI_COMS_NET_AUTH_TOKEN="$(openssl rand -hex 32)"
export PI_COMS_NET_PORT=52965
PI_COMS_NET_HOST=0.0.0.0 pi-coms-net-server
```

Clients:

```bash
export PI_COMS_NET_SERVER_URL=http://host:52965
export PI_COMS_NET_AUTH_TOKEN=...same-token...
pi-coms-net --name laptop-dev
```

Tools exposed:

```text
coms_net_list
coms_net_send
coms_net_get
coms_net_await
```

Security: use auth token always off-localhost. Put TLS/reverse proxy in front for remote use.

## Update process

Source repo is not installed as package. To update:

1. Clone `https://github.com/disler/pi-vs-claude-code`.
2. Copy chosen files into `modules/home-manager/pi-extensions/pi-vs-cc/`.
3. Patch imports:
   - `@mariozechner/pi-coding-agent` → `@earendil-works/pi-coding-agent`
   - `@mariozechner/pi-tui` → `@earendil-works/pi-tui`
   - `@mariozechner/pi-ai` → `@earendil-works/pi-ai`
   - `@sinclair/typebox` → `typebox`
4. Run `nix flake check` or deploy.
