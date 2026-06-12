# `gwt-clean` — Design

## Problem

The `gwt` command in `~/.bashrc` creates git worktrees under `<repo>/worktrees/<branch>`. Over time these accumulate and consume significant disk space. There is no cleanup command, so stale, merged, or abandoned worktrees must be removed manually.

## Goal

Add a `gwt-clean` bash function to `~/.bashrc` (source of truth: `~/.dotfiles/.bashrc`, symlinked to `~/.bashrc`) that removes worktrees which are safe to delete, with a dry-run by default so the user can inspect candidates before committing.

## Non-goals

- Cross-repo cleanup (current repo only).
- Cleaning up orphaned directories that git doesn't know about (rely on `git worktree prune` for metadata hygiene only).
- Escalating from `git branch -d` to `git branch -D` (never force-delete branches).

## Command

A shell function added to `~/.bashrc` below the existing `gwt` function.

```
Usage: gwt-clean [--force] [--stale-days N]

  --force          Actually delete. Without this, runs in dry-run mode.
  --stale-days N   Override the 120-day stale threshold.
```

Runs from inside any git repo. Operates on that repo's `worktrees/` directory. The main/base worktree is never a candidate (not under `worktrees/`). The worktree the user is currently inside is never deleted.

## Classification

For each worktree under `<repo>/worktrees/`, classify as one of:

1. **DELETE** — clean **AND** (merged **OR** stale).
2. **KEEP: dirty** — has uncommitted changes, untracked files, no upstream, or unpushed commits.
3. **KEEP: active** — clean, not merged, mtime within the stale threshold.
4. **KEEP: current** — the worktree the user is currently `cd`'d into.

### "Clean" (all three required)

- `git status --porcelain` is empty (no staged/unstaged/untracked).
- No unpushed commits: `git rev-list @{u}..HEAD` is empty.
- Upstream is configured. If no upstream is set, the branch is treated as **not clean** (safer — could be unpushed local-only work).

### "Merged" (either is sufficient)

- Branch is reachable from the repo's default branch (`main` or `master`, whichever exists locally), per `git branch --merged`.
- OR the remote-tracking branch is `[gone]` per `git for-each-ref`, after a `git fetch --prune` at the start of the run. This catches squash-merged and rebase-merged PRs.

### "Stale"

The newest file mtime anywhere inside the worktree is older than 120 days (overridable via `--stale-days N`). Implementation: `find <worktree> -type f -exec stat -f '%m' {} + | sort -n | tail -1`, compare against `now - stale_days * 86400`.

Rationale: directory mtime alone would miss the case where the user edits existing files (doesn't touch entries at the top level). "Newest file anywhere in the tree" reflects actual engagement with the worktree. The `find` is O(N files) but only runs once per worktree during classification, not per-file.

## Behavior

### Startup

1. Resolve the repo root the same way `gwt` does: `git rev-parse --git-common-dir`, then `dirname`.
2. Run `git fetch --prune` once, printing a note. If it fails (no network, no remote), warn and continue with local info only.
3. Enumerate worktrees via `git worktree list --porcelain`, filtering to paths under `<repo>/worktrees/` (same awk pattern as `_gwt_list`).

### Output (dry-run)

```
DELETE        feat/new-checkout         merged, clean, 8d old
DELETE        exp/old-spike             stale (186d), clean
KEEP: dirty   fix/payment-bug           unpushed commits (3)
KEEP: active  feat/wip                  clean, 14d old, unmerged
KEEP: current chore/this-branch         you are here

5 worktrees: 2 will be deleted, 3 will be kept
Reclaimable: ~340 MB

(dry run — pass --force to delete)
```

Disk usage is computed via `du -sh` per worktree. On macOS, `du` does not follow symlinks by default, so no extra flags are needed.

### Output (`--force`)

For each DELETE worktree:
1. `git worktree remove <path>`. If that fails (lock, etc.), warn and skip — do not escalate to `--force`.
2. If the branch was classified as merged, run `git branch -d <branch>`. If `-d` fails (unmerged locally), warn and skip. Stale-but-unmerged branches are never deleted.

After processing, run `git worktree prune` to clean up metadata for anything removed.

Final line: `deleted N worktrees, freed ~X MB`.

## Edge cases

- **Not in a git repo** → error with the same message style as `gwt`.
- **No `worktrees/` directory** → print "no worktrees to clean" and exit 0.
- **Default branch missing** → if neither `main` nor `master` exists locally, skip the `--merged` check; rely on `[gone]` detection only; print a note.
- **Orphaned directories** (`worktrees/X` exists but git doesn't list it) → ignored. Not this tool's job. `git worktree prune` handles the inverse case (git knows about a worktree whose directory is gone).
- **Branch names with `/`** → handled correctly by `git worktree remove` and by the existing awk-based path extraction.
- **No upstream configured** → classified as `KEEP: dirty` with reason `no upstream`.
- **`git fetch` fails** → warn, continue. `--merged` still works from local refs.

## Exit codes

- `0` — success (dry-run or `--force`), including the "no worktrees" case.
- Non-zero — unexpected errors (not in a git repo, etc.).

## Out of scope / future

- `--yes`/batch-with-confirmation mode.
- Global mode (scan multiple parent directories).
- Making `gwt-clean` a subcommand of `gwt`.
- Cleaning orphan directories git has lost track of.

These can be added later if the simple version proves insufficient.
