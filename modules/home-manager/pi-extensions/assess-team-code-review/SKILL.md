---
name: assess-team-code-review
description: analyze code review participation and quality by developer
---

# Soul

Analyze code review patterns — who reviews, who doesn't, review quality, turnaround time.

# Analysis Protocol

## Phase 1: Review Data Collection

Use GitHub/GitLab API or CLI to gather:
- Review requests by author
- Reviews given by author
- Review turnaround time
- Review comments per PR

If API not available, use commit metadata:
- Co-authored commits (indicate collaboration)
- Reviewed-by footers in commits

## Phase 2: Review Attribution

Score each author:
- Reviews given vs received
- Review depth (comments, suggestions)
- Turnaround speed
- Review participation rate

## Phase 3: Collaboration Patterns

Assess:
- Who reviews the most?
- Who rarely reviews?
- Review distribution (even vs lopsided)
- Cross-team reviewing patterns

# Output Format

Save to: `docs/assessment/team-<YYYY-MM-DD>-code-review.md`

```markdown
# Team Assessment: Code Review

## Review Participation
| Author | Reviews Given | Reviews Received | Participation Rate | Score |
|--------|---------------|-----------------|--------------------|-------|
| ... | N | N | X% | A-F |

## Review Turnaround
| Author | Avg Time to Review | Fast Reviews % | Quality |
|--------|--------------------|----------------|---------|
| ... | X hours | X% | A-F |

## Review Distribution
[Analysis of review load distribution]

## Collaboration Patterns
| Author | Cross-team Reviews | Co-authored Commits |
|--------|-------------------|---------------------|
| ... | N | N |

## Key Findings
1. [Finding with evidence]
2. [Finding with evidence]

## Score: A-F (one line rationale)

## Recommendations
1. [Priority]
2. [Secondary]
```

# Return Summary

Return max 10 lines:
- Top reviewers
- Review distribution insights
- Collaboration patterns
- Score: A-F with one-line rationale