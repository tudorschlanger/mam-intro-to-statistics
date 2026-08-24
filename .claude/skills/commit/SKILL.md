---
name: commit
description: Stage, commit, and push to main. Use for the standard commit cycle.
argument-hint: "[optional: commit message]"
allowed-tools: ["Bash", "Read", "Glob"]
---

# Commit and Push

Stage changes, commit with a descriptive message, and push. Creates a PR if on a feature branch.

## Steps

1. **Check current state:**

```bash
git status
git diff --stat
git log --oneline -5
git branch --show-current
```

2. **Stage files** — add specific files (never use `git add -A`):

```bash
git add <file1> <file2> ...
```

Do NOT stage `.claude/settings.local.json` or any files containing secrets.

3. **Commit** with a [Conventional Commits](https://www.conventionalcommits.org/) message:

If `$ARGUMENTS` is provided, use it as the commit message. Otherwise, analyze the staged changes and write a message:

```
<type>[(scope)]: <description>
```

**Types:** `feat`, `fix`, `docs`, `refactor`, `test`, `style`, `chore`, `perf`, `data`, `slides`
**Scope** (optional): a noun in parentheses — e.g., `fix(panel):`, `slides(lecture3):`

```bash
git commit -m "$(cat <<'EOF'
<type>: <description>
EOF
)"
```

4. **Push and optionally create PR:**

**If on `main`:**
```bash
git push origin main
```

**If on a feature branch:**
```bash
git push -u origin <branch-name>
gh pr create --title "<type>: <description>" --body "$(cat <<'EOF'
## Summary
<2-3 bullet points>

## Quality
- Score: <N>/100
- Review agents run: <list>

## Files Changed
<list of modified files>
EOF
)"
```

5. **Report** what was committed, pushed, and (if applicable) the PR URL.

## When to Use Branches

This is decided during plan mode (see `plan-first-workflow.md`):

- **Minor** (single file, small fix, docs) → commit directly to `main`
- **Major** (multi-file feature, new lecture, risky refactor) → create a branch, work there, PR when done

Branch naming: `<type>/<short-description>` (e.g., `feat/lecture-5-iv`, `fix/panel-clustering`)

## Important

- Exclude `settings.local.json` and sensitive files from staging
- If the commit message from `$ARGUMENTS` is provided, use it exactly
- If the push is rejected (diverged history), ask the user whether to pull first or force-push
- PRs require score >= 90/100; direct-to-main requires >= 80/100
