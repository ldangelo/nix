---
name: obsidian
description: "Obsidian Vault Integration — Automatically save session logs to your Obsidian vault. Enables persistent AI agent memory across pi sessions."
---

# Obsidian Integration Skill

This skill provides Obsidian vault integration for the pi agent, enabling persistent context and automatic session log saving.

## Vault Configuration

The Obsidian vault path is configured as:

```
VAULT="/Users/ldangelo/Library/Mobile Documents/iCloud~md~obsidian/Documents/ldangelo"
```

### Directory Structure

- **Sessions/** - Session logs saved at end of each session
- **Topics/** - Topic notes for conceptual knowledge
- **Daily Notes/** - Date-based journal entries
- **Projects/** - Project-specific documentation
- **Tasks/** - Task tracking and action items

## When to Use

Use this skill (automatically) when:

1. **Saving Session Logs** - At the end of every session (automatic)
2. **Searching for Context** - Looking up concepts, projects, or past work
3. **Saving Documents** - When asked to "save to obsidian"
4. **Managing Tasks** - Creating task notes in the vault
5. **Looking up Information** - Querying the vault for previous work

## Automatic Session Log Saving

Session logs are **automatically saved** to the Obsidian vault at session end via the `obsidian-session-saver.ts` extension.

The extension:
- Listens to `session_shutdown` events
- Extracts conversation highlights from the session
- Generates a markdown file with metadata (date, project, tags)
- Saves to `Sessions/YYYY-MM-DD-session-id.md`

**Automatic Saving Behavior:**
- Triggered when pi exits normally (Ctrl+C, Ctrl+D)
- Does NOT trigger on `/reload`, `/new`, or `/resume` (only on actual quit)
- Session file path determines the output filename
- Context is extracted from the last user/assistant messages

### How It Works

1. On `session_shutdown` event, the extension extracts:
   - Session header metadata (date, working directory)
   - Last user message content
   - Last assistant message content
   - Tool call count
   - Total entries

2. Generates a markdown file with:
   ```markdown
   ---
   date: YYYY-MM-DD
   project: project-name
   tags: [pi-session, auto-saved]
   ---
   
   # Session — YYYY-MM-DD
   ...
   ```

3. Saves to `VAULT/Sessions/session-id.md`

## Helper Scripts

The skill includes helper scripts for manual operations:

### Search Vault

```bash
# Search for a term across all notes
pi obsidian search "term"

# Search in specific directory
pi obsidian search "term" --dir=Projects

# Search with filters
pi obsidian search "term" --dir=Sessions --date=2024
```

### Get Vault Context

```bash
# Get recent session logs
pi obsidian recent-sessions

# Get topics list
pi obsidian list-topics

# Get daily notes for today
pi obsidian daily-notes
```

### Save Document to Vault

```bash
# Save content to a named file in the vault
pi obsidian save "filename.md" --content="..." --dir=Projects

# Save to daily notes
pi obsidian save-daily "content" --tags=tag1,tag2
```

### List Vault Contents

```bash
# List all topics
pi obsidian list-topics

# List recent sessions
pi obsidian list-sessions

# List daily notes
pi obsidian list-daily-notes
```

## Integration with Claude, OpenClaw, and Codex

All four agents (pi, Claude, OpenClaw, Codex) use the same Obsidian vault for persistent memory:

| Agent | Vault Path | Session Logs |
|-------|-----------|--------------|
| **pi** | `~/Library/Mobile Documents/iCloud~md~obsidian/Documents/ldangelo` | `Sessions/` |
| **Claude** | Same vault | `Sessions/` |
| **OpenClaw** | Same vault | `Sessions/` |
| **Codex** | Same vault | `Sessions/` |

## Session Log Format

```markdown
---
date: YYYY-MM-DD
project: project-name
tags: [pi-session, auto-saved]
---

# Session — YYYY-MM-DD

## Summary
This session log was automatically saved by the pi agent.

## Conversation Highlights
[Last user message content]

## Key Results
[Last assistant message content]

## Statistics
- Tool calls: N
- Total entries: N
- Reason: quit

## Open Items
*Review the session log and add any pending tasks here.*

## Files Modified
*Review the session log and list any files modified here.*

## Key Learnings
*Review the session log and note any important learnings here.*
```

## Configuration

### Environment Variables

- `PI_OBSIDIAN_VAULT` - Override the default vault path

### Extension Location

The session saver extension is at:
```
~/.pi/agent/skills/obsidian/obsidian-session-saver.ts
```

## Example Usage

```bash
# Session ends automatically - log saved to vault
pi obsidian save-session  # Manual save if needed

# Search the vault
pi obsidian search "api design patterns"

# List topics
pi obsidian list-topics

# Get recent sessions
pi obsidian recent-sessions
```

## Notes

- All agents share the same Obsidian vault for unified persistent memory
- Session logs are saved automatically at session end (quit only)
- The extension uses `session_shutdown` event for reliable saving
- Manual scripts available for search and context retrieval
