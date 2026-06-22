# Migrate from projectile to built-in project.el

**Date:** 2026-06-22
**Status:** Approved design, pending implementation plan

## Goal

Drop the third-party `projectile` dependency and use Emacs' built-in
`project.el` (plus the existing `consult` / `vertico` / `orderless` stack) for
all project navigation, **without disrupting the existing `C-c p` workflow or
key bindings**.

## Motivation

Lean on built-in Emacs + consult; fewer third-party packages to maintain.
`project.el` is now sufficient for this workflow (find-file, switch among many
projects, switch-buffer, project-wide replace) — the features projectile still
leads on (type-aware test/run/compile, non-VC project detection, large-repo
caching) are not used here, since all projects are git repos and project-wide
search already lives on a separate global binding (`C-c s` → `consult-ripgrep`).

## Current projectile footprint (in `init.el`)

| Concern | Location | Disposition |
| --- | --- | --- |
| `C-c p` → `projectile-command-map` | `use-package projectile` `:bind-keymap` | Re-point to `project-prefix-map` |
| `consult-project-function` → `projectile-project-root` | consult `:config` | Remove override (use native detection) |
| `copy-project-relative-path`, `create-python-import` | helper defuns | Switch to `project-root` helper |
| `projectile-mode` | projectile `:config` | Remove with the package |

Plus, in `bashrc.d/git-worktree.sh`, the `gwt` function auto-registers each new
worktree with projectile so it is instantly switchable in Emacs:

```bash
emacsclient -e "(projectile-add-known-project \"$(pwd)/\")"
```

`C-c b` / `C-x p b` already use `consult-project-buffer`, which is
project-agnostic and needs no change.

## Chosen approach: re-point `C-c p` at `project-prefix-map`

`project.el`'s prefix map already uses the same letter conventions as
projectile (`f` find-file, `p` switch-project, `b` buffer, `d` dir, `r`
replace, `g` grep, `c` compile, `k` kill, `v` vc-dir), so re-pointing the
existing `C-c p` prefix preserves muscle memory with almost no custom code.

A hand-built keymap mirroring projectile key-by-key was considered and rejected:
it offers exact control of every sub-key (e.g. routing a search key to
`consult-ripgrep`) but adds code to maintain and drifts from upstream defaults.
The letters needed here (`f` / `p` / `b` / `r`) map cleanly, and search is not
under this prefix, so the extra control buys nothing.

## Changes

### `init.el`

1. **Bind the prefix:** `(bind-key "C-c p" project-prefix-map)`.
2. **Direct switch:** `(setq project-switch-commands #'project-find-file)` so
   `C-c p p` jumps straight into find-file (projectile's behavior) instead of
   the default "choose an action" dispatch menu.
3. **Seed the project list (one-time):** port the existing
   `projectile-known-projects` into `project.el`'s `project-list-file` so every
   currently-known project *and worktree* appears in `C-c p p` immediately.
   Visiting a project/worktree auto-remembers it thereafter. (A shallow scan of
   the dev directory is insufficient because worktrees are nested under
   `<repo>/worktrees/<branch>`; porting the list captures them.)
4. **Native consult detection:** remove the `consult-project-function` override
   so consult uses its built-in `project.el`-based detection.
5. **Helper functions:** replace `projectile-project-root` in
   `copy-project-relative-path` and `create-python-import` with a small
   `my/project-root` helper built on `project-current` / `project-root`.
6. **Remove projectile:** delete the `use-package projectile` block; run
   `package-autoremove` to uninstall the now-unused package.

### `bashrc.d/git-worktree.sh`

7. Replace the projectile auto-register line (and its comment) in `gwt` with the
   `project.el` equivalent, so newly created worktrees still auto-register for
   instant `C-c p p` switching:

   ```bash
   emacsclient -e "(project-remember-project (project-current nil \"$(pwd)/\"))"
   ```

   Update any `bats` test in `tests/` that asserts the old emacsclient call,
   following the repo's TDD convention (failing test first, then change).

## Validation performed (on this machine, Emacs 30.1)

- **Worktree-as-root is identical to projectile.** For the main repo, a
  worktree under `worktrees/<branch>`, and a nested slash-branch worktree,
  `project-root` returns the worktree itself — matching `projectile-project-root`
  exactly. Each worktree is its own project, remembered separately, with
  find-file scoped to that worktree.
- **Find-file speed is a non-issue.** Listing files in a large (~27k-file) git
  repo via `project-files` completed in ~0.31s uncached (`git ls-files`).
- **`gwt` replacement confirmed.** `(project-remember-project (project-current
  nil DIR))` adds a worktree to `project-known-project-roots` and is the direct
  analog of `projectile-add-known-project`.

## What we give up

Nothing used in this workflow. Projectile's remaining advantages — type-aware
`test`/`run`/`compile` defaults, broader non-VC project detection, and explicit
file caching for very large repos — are unused here. If type-aware commands are
ever wanted, `projection` (a projectile-like layer built on `project.el`) is the
escape hatch and does not require reverting this migration.

## Rollback

The change is config-only and reversible: restore the `use-package projectile`
block, re-point `C-c p`, reinstall projectile, and revert the
`git-worktree.sh` line. The persisted `project-list-file` is harmless if
projectile is restored.

## Out of scope

- Adding project-type-aware test/run/compile commands (`projection`, etc.).
- Changing project-wide search (`C-c s` → `consult-ripgrep` stays as-is).
- Changing `C-c b` / `C-x p b` buffer switching (already consult-based).
