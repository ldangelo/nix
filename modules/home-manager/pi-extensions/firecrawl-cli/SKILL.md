---
name: firecrawl-cli
description: Lightweight Firecrawl web scrape/search/map via pi-firecrawl CLI. Use for web scraping, Firecrawl search, page extraction, sitemap URL discovery, or structured page data when MCP tool schemas are too token-heavy.
---

# Firecrawl CLI

Use bash CLI. Do not call Firecrawl MCP tools directly unless CLI fails.

## Commands

```bash
pi-firecrawl scrape <url> --format markdown
pi-firecrawl scrape <url> --format json --prompt 'extract prices' --schema '{"type":"object"}'
pi-firecrawl search '<query>' --limit 5
pi-firecrawl search '<query>' --limit 5 --scrape
pi-firecrawl map <url> --search 'webhook docs' --limit 20
pi-firecrawl extract <url> --prompt 'extract product details' --schema '{"type":"object"}'
```

## Rules

- Specific fields/data → `--format json` with schema.
- Full page/article → `--format markdown`.
- Unknown URL → `search` first, then `scrape` selected URL.
- SPA empty result → retry `scrape --wait 5000`, then `map --search`, then `search`.
- Output can be large. Use low `--limit` and focused prompts.
