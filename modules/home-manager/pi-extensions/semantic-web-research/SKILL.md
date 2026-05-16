---
name: semantic-web-research
description: >
  Broad semantic internet discovery using already-configured Exa and Firecrawl
  tools. Use for research topics, seed URLs, or mixed topic+URL requests where
  the goal is discovering, clustering, ranking, and preserving source material
  rather than producing a single final factual answer.
---

# Semantic Web Research

Use this skill to run a prompt-only research pipeline with Exa and Firecrawl.
Do not call Firecrawl or Exa APIs directly from code. Do not manage credentials,
install packages, create a database, or start an open-ended crawler. Use only
tools already available in the current Pi Agent runtime.

## Goal

Optimize for broad semantic discovery:

- Coverage: find multiple relevant source clusters.
- Diversity: include varied source types and viewpoints.
- Novelty: surface useful sources beyond obvious keyword-search results.
- Gaps: state what remains underexplored.

The primary deliverable is a research map plus reusable artifacts for later
agents. A polished final answer is optional and secondary.

## Tool Roles

- Exa: semantic source discovery, related-source expansion, conceptual breadth,
  source novelty, and query reformulation.
- Firecrawl: fetch readable page content, crawl bounded seed URLs, extract page
  text/metadata, and enrich selected Exa results.

If one provider is unavailable, continue only when useful and explicitly state
which capability is degraded:

- Exa missing: semantic breadth and novelty are degraded.
- Firecrawl missing: page evidence extraction and crawl enrichment are degraded.
- Both missing: stop and report that this skill requires Exa or Firecrawl tools.

## Input Modes

Classify the request before retrieval.

### Topic-Only

Use when the user provides a topic, question, market, technology, person,
company, or research area with no seed URLs.

Process:

1. Use Exa first with 2-4 semantically varied searches.
2. Collect a broad candidate set of URLs, titles, snippets, authors, dates, and
   source types when available.
3. Select promising, diverse, and non-duplicate URLs.
4. Use Firecrawl to fetch or extract the selected pages.
5. Cluster, rank, and identify gaps.

### URL-Only

Use when the user gives one or more seed URLs but no broader topic.

Process:

1. Use Firecrawl first to fetch or crawl the seed URLs with a bounded depth.
2. Extract page text, links, metadata, and recurring concepts.
3. Use Exa to find semantically related sources around extracted concepts.
4. Merge seed-derived and Exa-derived sources.
5. Cluster, rank, and identify gaps.

### Topic + URLs

Use when the user gives both research framing and seed URLs.

Process:

1. Use Firecrawl on the seed URLs to understand the local source context.
2. Use Exa to search around both the original topic and concepts extracted from
   the seed pages.
3. Merge both streams.
4. Dedupe, cluster, rank, and identify gaps.

## Retrieval Discipline

- Keep crawls bounded and purposeful. Do not create long-running autonomous
  crawl loops.
- Prefer breadth first, then selective enrichment.
- Dedupe by canonical URL, domain, title similarity, and near-identical snippets.
- Track when sources are inaccessible, thin, duplicate, paywalled, stale, or
  off-topic.
- Preserve source dates when available; flag undated sources.
- Do not overclaim from snippets alone. Mark snippet-only items clearly.

## Ranking

Rank sources with a balanced score:

- Relevance to user topic.
- Semantic novelty.
- Source diversity.
- Evidence richness after Firecrawl extraction.
- Recency when time matters.
- Authority or primary-source status when applicable.

Do not let familiar high-authority domains crowd out useful niche sources.

## Output

Return a concise research brief:

```markdown
## Research Map

### Retrieval Strategy
- Input mode:
- Exa role:
- Firecrawl role:
- Provider gaps:

### Source Clusters
1. Cluster name
   - Why it matters:
   - Representative sources:

### Top Sources
| Rank | Source | Type | Why useful | Evidence status |
| --- | --- | --- | --- | --- |

### Novel Leads
- Source or query lead — why it may expand discovery.

### Gaps
- Missing angle, source type, geography, timeframe, stakeholder, or keyword.

### Suggested Follow-up Queries
- Query
```

When the user asks for saved artifacts, or when the research is substantial,
also create a markdown artifact in the workspace. Choose a practical path such
as `.omx/research/<slug>.md` unless the user specifies another path.

Artifact should include:

- Original request.
- Input mode.
- Exa searches and result candidates.
- Firecrawl fetched URLs and extraction status.
- Raw URLs and metadata.
- Extracted snippets/passages when available.
- Dedupe notes.
- Source clusters.
- Ranking rationale.
- Gaps and follow-up queries.

## Non-goals

- No direct Firecrawl or Exa API implementation.
- No credential management.
- No new scripts or dependencies.
- No database or persistent index.
- No browser UI.
- No provider fallback beyond Firecrawl and Exa.
- No guaranteed final factual answer.

