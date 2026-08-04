---
name: implement-issue
trigger: implement issue / work on #NNN
description: Implements a GitHub issue end-to-end. Use when asked to implement, work on, or fix a specific issue number.
compatibility: Requires the `gh` CLI and an authenticated GitHub session.
---

# Implement a GitHub issue

This skill wraps the Standard Workflow defined in `AGENTS.md`. Run the steps below before and after that workflow.

## Before the standard workflow

**A. Read the issue in full:**

```bash
gh issue view {number}
```

If `gh` is not authenticated, stop and ask the user to authenticate before continuing.
If the issue is already closed or has a linked merged PR, stop and inform the user before proceeding. Ask whether to continue anyway.

**B. Check/create the branch:**

- If on `main`: `usethis::pr_init("fix-{number}-{description}")`
- If `usethis::pr_init(...)` fails or is unavailable, fall back to `git checkout -b fix-{number}-{description}` and inform the user that `usethis` was not used.
- If currently on a branch that is not `main` and does not match `fix-{number}-*`, stop and ask the user whether to switch to `main` and create a new branch or continue on the current branch.
- Branch format: `fix-{number}-{description}`
   - The three top-level segments (`fix`, `{number}`, `{description}`) are separated by hyphens; within the `{description}` segment, use snake_case.
  - Example: `fix-42-validate_input`
- If a branch matching the pattern `fix-{number}-*` already exists locally or on the remote, check it out instead of creating a new one.

## Run the Standard Workflow from AGENTS.md

Steps 1–9 of the Standard Workflow are the core development loop.

## After the standard workflow

**C. Commit and push:**

1. Review commits already on this branch (not on `main`) — these are all part of the same PR and should inform the PR description:
   ```bash
   git log main..HEAD --oneline
   ```
2. Stage and commit all changes:
   If `git status` shows no changes to commit, skip the commit step and note this to the user.
   ```bash
   git add -A
   git commit -m "{short imperative summary}"
   ```
3. Push the branch:
   ```bash
   git push -u origin HEAD
   ```
   If push is rejected, report the error and stop.
4. Open the PR:
   ```bash
   gh pr create --fill
   ```
   Use `--title` and `--body` explicitly if the `--fill` output omits the issue number, does not summarize the changes made, or is shorter than two sentences.

This step may be overridden — the user may ask you to stop before committing, handle the push themselves, or complete only part of the workflow. Always follow explicit user instructions over these defaults.
