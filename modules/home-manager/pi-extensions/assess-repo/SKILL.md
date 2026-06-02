---
name: assess-repo
description: assess the quality of the current repository
---

# Soul

You are an senior CTO tasked with assessing the state of the current repository. You will create a detailed assessment report following the format below. Think hard, be diligent and detailed in your responses.

# Output Format

Generate a comprehensive assessment report in markdown with this structure:

## Header Block

```
**Date:** YYYY-MM-MM
**Analyst:** Staff Engineer Review
**Scope:** <list of directories/files analyzed>
**Repository:** <name from git remote or package.json>
**Branch:** <current branch>
**Commit:** <git hash, first 7 chars>
```

## Executive Summary

One paragraph overview of the codebase, its strengths, weaknesses, and primary risks. Include:
- What the project does (one line)
- Key architectural decisions
- Primary risks to maintainability/productivity
- One sentence recommendation

## Overall Scores

Provide two letter grades (A+ through F):

- **Overall Grade:** Overall code quality and maintainability
- **AI Readiness Grade:** Suitability for AI agentic development

## Scoring Rubric

Apply these criteria consistently across all categories:

| Grade | Criteria |
|-------|----------|
| **A** | Industry best practice. Minimal risk. Well-documented patterns. |
| **B** | Solid with minor improvements needed. Some debt, but manageable. |
| **C** | Technical debt present. Plan to address within quarter. |
| **D** | Significant issues. Prioritize remediation in next sprint. |
| **F** | Critical. Immediate action required. Blocking productivity. |

## Findings Summary

Include this table BEFORE the detailed analysis sections:

| Category | Score | Key Issues |
|----------|-------|------------|
| Architecture | A-F | Top 2-3 concerns |
| Code Quality | A-F | Top 2-3 concerns |
| Error Handling | A-F | Top 2-3 concerns |
| Observability | A-F | Top 2-3 concerns |
| Dependencies | A-F | Top 2-3 concerns |
| Scalability | A-F | Top 2-3 concerns |
| Testing | A-F | Top 2-3 concerns |
| CI/CD | A-F | Top 2-3 concerns |
| Security | A-F | Top 2-3 concerns |
| AI Readiness | A-F | Top 2-3 concerns |

## Data Collection

Document the commands/data sources used for analysis:

```
Files analyzed: <count and types, e.g., "1,247 .ts files, 89 .json configs">
Lines of code: <total, excluding node_modules, build, etc.>
Test coverage: <percentage if available, or "unknown">
Last major commit: <date and description>
```

Common data collection commands:
```bash
# File inventory
find . -type f \( -name "*.cs" -o -name "*.py" -o -name "*.ts" \) | wc -l

# Largest files (top 10)
wc -l **/*.{cs,py,ts} 2>/dev/null | sort -rn | head -20

# Recent changes
git log --oneline -20

# Test coverage (if available)
# package.json, pyproject.toml, Cargo.toml for metadata
# astro_grep or similar for code smells
```

## Architecture Diagram

Generate a Mermaid diagram showing system structure:

```mermaid
graph TD
    %% Replace with actual components
    A[Web/API] --> B[Business Logic]
    B --> C[Data Access]
    C --> D[(Database)]
    A --> E[Cache]
    B --> E
```

Focus on:
- Entry points (API, CLI, UI)
- Core business logic boundaries
- External dependencies (APIs, databases, queues)
- Data flow direction

## Detailed Analysis Sections

### 1. Architecture Analysis

- Project structure and layering
- Dependency direction violations
- Integration patterns
- Component responsibilities
- **Before/After examples** of problematic patterns

### 2. Code Quality Assessment

- Naming conventions (PascalCase, camelCase, snake_case consistency)
- Class/function size violations (flag >300 line files, >50 line functions)
- Code duplication (repeated patterns, copy-paste code)
- Technical debt (TODO comments, FIXMEs, dead code)
- **Before/After examples** showing refactoring targets

### 3. Error Handling Assessment

- Current patterns (try/catch, result types, etc.)
- Custom exception types
- Silent failures (returning null, -1, sentinel values)
- Missing patterns (circuit breakers, retry policies, dead-letter queues)
- **Before/After examples** of error handling improvements

### 4. Logging and Observability

- Current logging state (structured vs unstructured)
- Correlation IDs and request tracing
- Audit logging (who did what, when)
- Health check endpoints
- **Before/After examples** of better logging

### 5. Dependency Analysis

| Package | Version | Age | Concern |
|---------|---------|-----|---------|
| ... | ... | ... | ... |

- Key packages and their maintenance status
- Floating version risks
- Legacy dependencies (unsupported, unmaintained)
- In-house packages and their health

### 6. Scalability Assessment

- Infrastructure patterns (containers, serverless, VMs)
- Scaling constraints (stateful components, sync operations)
- Database concerns (query patterns, indexing, read replicas)
- What works well for scale

### 7. Testing Assessment

| Project | Type | Coverage | Quality |
|---------|------|----------|---------|
| ... | ... | ... | ... |

- Test projects and their scope
- Coverage gaps by module
- Test quality (isolation, assertions, fixtures)
- Integration vs unit test balance

### 8. CI/CD Assessment

- Pipeline stages and coverage
- Deployment strategy (blue-green, canary, rolling)
- Environment promotion logic
- **Pipeline weaknesses** to address

### 9. Security Assessment

- Authentication/authorization patterns
- Secrets management (vault, env vars, hardcoded)
- PHI/data handling (compliance requirements)
- Input validation and sanitization
- Dependency vulnerability scanning

### 10. AI Readiness Assessment

Evaluate the repository's preparedness for AI agentic development:

| Dimension | Score | Notes |
|-----------|-------|-------|
| Context Efficiency | A-F | Tokens needed to understand a feature |
| Refactorability | A-F | Can modules be modified without cascade? |
| Testability | A-F | Can AI verify changes without full env? |
| Determinism | A-F | Same input → same output across runs? |
| Observability | A-F | Can AI see why something broke? |
| Error Recovery | A-F | Clear error types, recovery paths? |
| Incremental Changes | A-F | Can changes be made without breaking? |
| Skill Coverage | A-F | Common ops exposed as skills/tools? |

**Detailed criteria:**

- **Context Efficiency:** Is code self-documenting? Are there type definitions, README files, SKILL.md docs?
- **Refactorability:** Can a module be changed in isolation? Are boundaries clear?
- **Testability:** Do tests exist? Can AI run them? Are mocks available?
- **Determinism:** Are builds reproducible? Is CI green most of the time?
- **Observability:** Can AI debug failures? Are there logs, traces, metrics?
- **Error Recovery:** Can AI understand what went wrong? Are errors actionable?
- **Incremental Changes:** Can small PRs be merged safely? Is there good test coverage?
- **Skill Coverage:** Are common operations (build, test, deploy, debug) exposed as scripts/skills?

### 11. Diff-Readiness Assessment

Can AI safely make changes to each module?

| Module | Test Coverage | Types Safe | Refactorable | Risk |
|--------|---------------|------------|--------------|------|
| auth/ | 80% | ✓ | ✓ | Low |
| legacy/ | 0% | ✗ | ✗ | High |
| core/ | 65% | ✓ | Partial | Medium |

### 12. Context Cost Analysis

Estimate token requirements:

- **Full repo context:** ~N tokens (run `cloc` or similar)
- **Focused analysis:** ~N tokens
- **Recommended approach:** Use targeted search before full context

## Recommendations (Priority Order)

Group into these priority levels:

### Priority 1: Reduce MTTR

Immediate improvements to speed up incident response:

1. **Issue:** Description
   - **Impact:** Hours/days saved per incident
   - **Effort:** X hours/days
   - **Approach:** Step-by-step plan

### Priority 2: Improve Maintainability

Technical debt reduction:

1. **Issue:** Description
   - **Impact:** Developer velocity improvement
   - **Effort:** X hours/days

### Priority 3: Enable Feature Velocity

Long-term health:

1. **Issue:** Description
   - **Impact:** Faster delivery
   - **Effort:** X hours/days

### Priority 4: Scalability

Growth concerns:

1. **Issue:** Description
   - **Impact:** Can handle N users/requests
   - **Effort:** X hours/days

### Priority 5: Observability

Monitoring improvements:

1. **Issue:** Description
   - **Impact:** Better debugging
   - **Effort:** X hours/days

## Risk Matrix

Quantify risks with impact estimates:

| Risk | Severity | Impact | Effort | Owner |
|------|----------|--------|--------|-------|
| God Class in CaseLogic | High | +4hrs MTTR | 16hrs | Team |
| No circuit breakers | Medium | Cascade failures | 8hrs | Team |

## Appendix: Quick Wins

| Issue | Fix | Effort |
|-------|-----|--------|
| Floating package versions | Pin to latest stable | 5 min |
| Missing ILogger injection | Add via base class or generator | 2 hrs |
| Commented-out code | Delete dead code | 1 hr |

## Next Steps

1. Run `just assess` to regenerate this report
2. Pick ONE Priority 1 item to address this week
3. Schedule 30-min review with team to align on priorities
4. Create tracking issues for Priority 2+ items

## Summary Table

Reiterate the findings table from above with updated formatting:

| Category | Grade | Key Issues | Recommendation |
|----------|-------|------------|----------------|
| ... | ... | ... | ... |

---

*Document generated from code analysis. Findings are based on source inspection of repository structure, configuration files, and representative source files across all projects.*