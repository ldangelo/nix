# Semantic Web Research

Prompt-only Pi Agent skill for broad semantic internet discovery with
already-configured Exa and Firecrawl tools.

## Purpose

Use this for research where the first need is a broad source map: clusters,
novel leads, gaps, and reusable artifacts for later agents.

## Behavior

- Topic-only input: Exa discovers sources, Firecrawl enriches selected pages.
- URL-only input: Firecrawl extracts seed URLs, Exa expands around concepts.
- Topic plus URLs: both streams run, then merge, dedupe, cluster, and rank.

## Boundary

This skill does not implement API clients, manage keys, add dependencies, create
databases, or run unbounded crawlers.

