#!/bin/zsh
# obsidian-session-saver.sh — SessionEnd hook for pi agent
# Saves session logs to Obsidian vault automatically at session end.
# This ensures persistent context across pi agent sessions.

VAULT="/Users/ldangelo/Library/Mobile Documents/iCloud~md~obsidian/Documents/ldangelo"
SESSIONS_DIR="$VAULT/Sessions"
SESSION_LOG="$SESSIONS_DIR/${SESSION_ID}.md"

# Get session info from the JSONL log
SESSION_FILE="$SESSION_LOG"

if [ -f "$SESSION_FILE" ]; then
    # Extract session metadata
    SESSION_ID=$(basename "$SESSION_FILE" .jsonl)
    PROJECT_NAME=$(basename "$(dirname "$SESSION_FILE")")
    FIRST_USER_MSG=$(grep -m1 '"type":"user"' "$SESSION_FILE" 2>/dev/null | jq -r '.message.content // "No content"' 2>/dev/null | head -c 100)
    
    # Extract tags from session environment or generate from content
    TAGS="session-log,auto-saved"
    
    # Extract date from session filename
    SESSION_DATE=$(echo "$SESSION_ID" | grep -oP '\d{4}-\d{2}-\d{2}' | head -1)
    [ -z "$SESSION_DATE" ] && SESSION_DATE=$(date +%Y-%m-%d)
    
    # Get project from session context if available
    PROJECT="pi-agent"
    if [[ "$SESSION_FILE" == *"Projects"* ]]; then
        PROJECT=$(basename "$(dirname "$SESSION_FILE")" | sed 's/^-//')
    fi
    
    # Create the markdown file
    cat > "$SESSION_FILE.md" << EOF
---
date: ${SESSION_DATE}
project: ${PROJECT}
tags: [${TAGS}]
---

# Session — ${SESSION_DATE}

## Summary
This session log was automatically saved by the pi agent.

## Conversation Highlights
$(grep -m5 '"type":"user"' "$SESSION_FILE" 2>/dev/null | jq -r '.message.content // empty' 2>/dev/null | head -c 2000)

## Open Items
*Review the session log and add any pending tasks here.*

## Files Modified
*Review the session log and list any files modified here.*

## Key Learnings
*Review the session log and note any important learnings here.*
EOF

    echo "✓ Session log saved to: $SESSION_FILE.md"
else
    # Create a stub if no session file found
    TODAY=$(date +%Y-%m-%d)
    STUB_FILE="$SESSIONS_DIR/${TODAY}-pi-session-stub.md"
    
    cat > "$STUB_FILE" << EOF
---
date: ${TODAY}
project: pi-agent
tags: [session-log, stub]
---

# Session Stub — ${TODAY}

## Summary
Session ended without a full log. This stub was auto-created.

## Open Items
- Review what was worked on and create a proper session log if needed.
EOF

    echo "✓ Session stub saved to: $STUB_FILE"
fi

exit 0
