#!/bin/zsh
# obsidian-search.sh — Search Obsidian vault for context
# Usage: obsidian-search.sh "search-term" [options]

VAULT="/Users/ldangelo/Library/Mobile Documents/iCloud~md~obsidian/Documents/ldangelo"
DEFAULT_DIR="Sessions"

usage() {
    echo "Usage: obsidian-search.sh \"search-term\" [options]"
    echo ""
    echo "Options:"
    echo "  --dir=DIR       Search in specific directory (default: Sessions)"
    echo "  --file=PATTERN  Search in files matching pattern"
    echo "  --case-sensitive  Enable case-sensitive search"
    echo "  --limit=NUM     Limit results (default: 10)"
    echo "  --json          Output in JSON format"
    echo "  --help          Show this help"
    echo ""
    echo "Examples:"
    echo "  obsidian-search.sh \"authentication\""
    echo "  obsidian-search.sh \"api design\" --dir=Topics"
    echo "  obsidian-search.sh \"terraform\" --file=*.tf --dir=Projects"
    exit 0
}

# Parse arguments
SEARCH_TERM=""
SEARCH_DIR="$DEFAULT_DIR"
FILE_PATTERN="*.md"
CASE_SENSITIVE=false
LIMIT=10
JSON_OUTPUT=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --dir=*)
            SEARCH_DIR="${1#--dir=}"
            shift
            ;;
        --file=*)
            FILE_PATTERN="${1#--file=}"
            shift
            ;;
        --case-sensitive)
            CASE_SENSITIVE=true
            shift
            ;;
        --limit=*)
            LIMIT="${1#--limit=}"
            shift
            ;;
        --json)
            JSON_OUTPUT=true
            shift
            ;;
        --help|-h)
            usage
            ;;
        *)
            if [ -z "$SEARCH_TERM" ]; then
                SEARCH_TERM="$1"
            fi
            shift
            ;;
    esac
done

if [ -z "$SEARCH_TERM" ]; then
    echo "Error: Search term required"
    usage
fi

# Construct search command
if [ "$CASE_SENSITIVE" = true ]; then
    RG_ARGS="-S --hidden"
else
    RG_ARGS="--hidden"
fi

# Search in the specified directory
DIR_PATH="$VAULT/$SEARCH_DIR"
RESULTS_FILE=$(mktemp)

if [ -d "$DIR_PATH" ]; then
    if [ "$JSON_OUTPUT" = true ]; then
        rg $RG_ARGS -n --json "$SEARCH_TERM" "$DIR_PATH" 2>/dev/null | head -n $((LIMIT * 100)) | jq -s 'limit('$LIMIT'; .)' 2>/dev/null
    else
        rg $RG_ARGS --color=always -n -C 2 --heading "$SEARCH_TERM" "$DIR_PATH" 2>/dev/null | head -n $((LIMIT * 50))
    fi
else
    echo "Directory not found: $DIR_PATH"
    exit 1
fi

rm -f "$RESULTS_FILE"
