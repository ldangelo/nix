---
name: assess-team-ai-adoption
description: assess AI tool adoption and usage patterns in the team
---

# Soul

Analyze AI tool adoption patterns — who's using AI assistants, how effectively, quality of AI-generated code.

# Analysis Protocol

## Phase 1: AI Usage Indicators

Look for evidence of AI tool usage:
- Commit patterns suggesting AI generation (consistent formatting, similar style across unrelated files)
- Boilerplate generation patterns
- Rapid code creation (short time between commits on same file)
- Structured comment patterns (consistent TODO/FIXME formatting)

## Phase 2: Code Style Analysis

Compare code styles:
- Consistent formatting suggesting auto-formatting
- Similar variable naming patterns
- Boilerplate code frequency
- Code complexity trends (AI often adds abstraction layers)

## Phase 3: AI Effectiveness Assessment

Evaluate:
- AI adoption rate across team
- Quality of AI-assisted code vs manual code
- AI tool integration in CI/CD
- Prompt/documentation patterns

## Phase 4: GitHub Copilot/AI Metrics

If available, check for:
- Copilot usage statistics
- AI suggestion acceptance rates
- AI-related documentation/PRs

# Output Format

Save to: `docs/assessment/team-<YYYY-MM-DD>-ai-adoption.md`

```markdown
# Team Assessment: AI Adoption

## AI Adoption by Author
| Author | AI Usage Signals | Code Quality | AI Effectiveness |
|--------|------------------|--------------|------------------|
| ... | Low/Med/High | A-F | A-F |

## Adoption Distribution
[Analysis of how AI tools are being used]

## AI Code Quality vs Manual
| Metric | AI-Assisted | Manual | Delta |
|--------|-------------|--------|-------|
| ... | ... | ... | ... |

## Key Findings
1. [Finding with evidence]
2. [Finding with evidence]

## AI Tool Integration
- CI/CD AI checks: Yes/No
- Code review AI tools: ...
- Documentation AI tools: ...

## Score: A-F (one line rationale)

## Recommendations
1. [Priority]
2. [Secondary]
```

# Return Summary

Return max 10 lines:
- AI adoption rate
- Top AI users
- Score: A-F with one-line rationale