# Projectile → project.el Migration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the third-party `projectile` package with built-in `project.el` while preserving the `C-c p` workflow and worktree-as-root behavior.

**Architecture:** Re-point the existing `C-c p` prefix at `project-prefix-map` (whose letter conventions already match projectile), tune `project-switch-commands` for projectile-style direct find-file, port the known-projects list into `project.el`, repoint two helper functions and the `gwt` shell hook off `projectile-project-root`/`projectile-add-known-project`, then remove the package.

**Tech Stack:** Emacs 30.1 (`project.el`, `consult`), Bash (`bashrc.d/git-worktree.sh`), git worktrees.

## Global Constraints

- **Public repo — no secrets/PII/company specifics in committed files.** Use generic placeholders; the `gwt` hook uses `$(pwd)` (generic). (`~/.dotfiles/CLAUDE.md`)
- **Preserve `C-c p` bindings:** `C-c p f` (find-file), `C-c p p` (switch-project), `C-c p b` (buffer), `C-c p r` (replace) must keep working.
- **All projects are git repos; worktrees live at `<repo>/worktrees/<branch>` and each must remain its own project root.**
- **Verification is via `emacsclient` against the running server** (no emacs test framework); editing `init.el` does not affect the running process, so each task also evals the equivalent form live to verify before relying on a restart.
- **Shell changes follow the repo's bats/TDD convention**; note that no existing test covers the `gwt` emacsclient hook.
- `init.el` is symlinked from `~/.emacs.d/init.el`; edit the real file `~/.dotfiles/init.el`.

---

### Task 1: Port projectile's known-projects into project.el (one-time runtime)

Runtime data migration only — **no repo file changes, no commit.** Must run while
projectile is still loaded in the running emacs (it currently holds 211 known
projects). Seeds `~/.emacs.d/projects` so all projects/worktrees appear in
`C-c p p` immediately.

**Files:**
- Writes (runtime): `~/.emacs.d/projects` (project-list-file)

**Interfaces:**
- Produces: a populated `project-known-project-roots` (~211 entries) for later manual verification.

- [ ] **Step 1: Record the before-count**

Run:
```bash
emacsclient -e '(progn (require (quote project)) (length (project-known-project-roots)))'
```
Expected: a small number (≈5).

- [ ] **Step 2: Run the one-time port**

Run:
```bash
emacsclient -e '(progn
  (require (quote projectile))
  (require (quote project))
  (let ((added 0))
    (dolist (d projectile-known-projects)
      (let ((dir (expand-file-name d)))
        (when (file-directory-p dir)
          (when-let ((pr (project-current nil dir)))
            (project-remember-project pr)
            (setq added (1+ added))))))
    (list (cons "added" added)
          (cons "known-roots-now" (length (project-known-project-roots))))))'
```
Expected: `added` is a few hundred; `known-roots-now` ≈ 211.

- [ ] **Step 3: Verify a worktree is in the list**

Run:
```bash
emacsclient -e '(seq-filter (lambda (r) (string-match-p "/worktrees/" r)) (project-known-project-roots))'
```
Expected: a non-empty list of worktree paths.

- [ ] **Step 4: Verify persistence to disk**

Run: `test -s ~/.emacs.d/projects && echo OK`
Expected: `OK` (file exists and is non-empty).

---

### Task 2: Swap the projectile use-package for native project.el config

**Files:**
- Modify: `~/.dotfiles/init.el` (the `(use-package projectile …)` block, currently near line 713)

**Interfaces:**
- Produces: `C-c p` bound to `project-prefix-map`; `project-switch-commands` set to `#'project-find-file`.

- [ ] **Step 1: Verify current binding (baseline)**

Run: `emacsclient -e '(format "%s" (key-binding (kbd "C-c p f")))'`
Expected: a projectile command (e.g. `projectile-find-file`).

- [ ] **Step 2: Replace the projectile block**

In `~/.dotfiles/init.el`, replace:
```elisp
(use-package projectile
  :defer t
  :ensure t
  :bind-keymap
    ("C-c p" . projectile-command-map)
  :config
    (setq projectile-completion-system 'auto)
    (projectile-mode)
  )
```
with:
```elisp
;; Built-in project.el replaces projectile. The C-c p prefix is re-pointed at
;; project-prefix-map, whose key letters already match projectile (f find-file,
;; p switch-project, b buffer, r replace, d dir, g grep, c compile, k kill,
;; v vc-dir). project-switch-commands makes C-c p p jump straight into
;; find-file like projectile did, instead of the action-menu default.
(use-package project
  :ensure nil
  :bind-keymap ("C-c p" . project-prefix-map)
  :config
  (setq project-switch-commands #'project-find-file))
```

- [ ] **Step 3: Apply the change live and verify the prefix**

Run:
```bash
emacsclient -e '(progn
  (require (quote project))
  (require (quote bind-key))
  (bind-key "C-c p" project-prefix-map)
  (setq project-switch-commands (function project-find-file))
  (list (cons "C-c p f" (format "%s" (key-binding (kbd "C-c p f"))))
        (cons "C-c p p" (format "%s" (key-binding (kbd "C-c p p"))))
        (cons "C-c p b" (format "%s" (key-binding (kbd "C-c p b"))))
        (cons "C-c p r" (format "%s" (key-binding (kbd "C-c p r"))))
        (cons "switch-cmds" (format "%s" project-switch-commands))))'
```
Expected: `project-find-file`, `project-switch-project`, `project-switch-to-buffer`, `project-query-replace-regexp`, and `switch-cmds` = `project-find-file`.

- [ ] **Step 4: Commit**

```bash
cd ~/.dotfiles
git add init.el
git commit -m "Replace projectile with native project.el (C-c p prefix)

Assisted by AI

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

### Task 3: Repoint integrations off projectile-project-root

**Files:**
- Modify: `~/.dotfiles/init.el` — consult `:config` (the two projectile lines, near line 92-93) and the two helper defuns (`copy-project-relative-path`, `create-python-import`, near lines 896-911)

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces: `my/project-root` (a zero-arg function returning the project root dir or nil).

- [ ] **Step 1: Remove the consult-project-function override**

In `~/.dotfiles/init.el`, inside the `consult` `:config`, delete these two lines:
```elisp
    (autoload 'projectile-project-root "projectile")
    (setq consult-project-function (lambda (_) (projectile-project-root)))
```
(Leave the rest of the consult `:config` intact.)

- [ ] **Step 2: Add the my/project-root helper and update the two defuns**

In `~/.dotfiles/init.el`, immediately above `(defun copy-project-relative-path …)`, add:
```elisp
(defun my/project-root ()
  "Return the current project root directory, or nil if not in a project."
  (when-let ((proj (project-current)))
    (project-root proj)))
```
Then in `copy-project-relative-path`, change:
```elisp
                  buffer-file-name (projectile-project-root))))
```
to:
```elisp
                  buffer-file-name (my/project-root))))
```
And in `create-python-import`, change:
```elisp
                  buffer-file-name (projectile-project-root))))) " import")))
```
to:
```elisp
                  buffer-file-name (my/project-root))))) " import")))
```

- [ ] **Step 3: Apply live and verify consult + helper**

Run:
```bash
emacsclient -e '(progn
  (require (quote project))
  (setq consult-project-function (function consult--default-project-function))
  (defun my/project-root () (when-let ((proj (project-current))) (project-root proj)))
  (let ((default-directory "/Users/ben/dev/whatnot_backend/worktrees/add-index-to-refund-rquest-context/"))
    (list (cons "consult-fn" (format "%s" consult-project-function))
          (cons "my/project-root" (my/project-root)))))'
```
Expected: `consult-fn` = `consult--default-project-function`; `my/project-root` = the worktree path (trailing slash).

- [ ] **Step 4: Verify init.el still has no unbalanced parens**

Run:
```bash
emacs --batch -Q --eval '(with-temp-buffer (insert-file-contents "~/.dotfiles/init.el") (emacs-lisp-mode) (condition-case e (progn (check-parens) (message "PARENS OK")) (error (message "PAREN ERROR: %s" e))))'
```
Expected: `PARENS OK`.

- [ ] **Step 5: Commit**

```bash
cd ~/.dotfiles
git add init.el
git commit -m "Repoint consult + path helpers off projectile-project-root

Assisted by AI

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

### Task 4: Update the gwt worktree hook to use project-remember-project

**Files:**
- Modify: `~/.dotfiles/bashrc.d/git-worktree.sh:89-90`

**Interfaces:**
- Consumes: `project-remember-project` / `project-current` (verified available in Task 1's environment).

- [ ] **Step 1: Replace the projectile hook**

In `~/.dotfiles/bashrc.d/git-worktree.sh`, replace:
```bash
      # add to emacs projectile
      emacsclient -e "(projectile-add-known-project \"$(pwd)/\")"
```
with:
```bash
      # register the new worktree with Emacs project.el (instant C-c p p switch)
      emacsclient -e "(project-remember-project (project-current nil \"$(pwd)/\"))" >/dev/null 2>&1
```

- [ ] **Step 2: Verify the emacsclient form is valid against the running server**

Run (uses an existing worktree path; harmless if already remembered):
```bash
emacsclient -e '(project-remember-project (project-current nil "/Users/ben/dev/whatnot_backend/worktrees/add-index-to-refund-rquest-context/"))' && echo FORM_OK
```
Expected: prints a value then `FORM_OK` (no error).

- [ ] **Step 3: Confirm the module still sources cleanly**

Run: `bash -n ~/.dotfiles/bashrc.d/git-worktree.sh && echo SYNTAX_OK`
Expected: `SYNTAX_OK`.

- [ ] **Step 4: Commit**

```bash
cd ~/.dotfiles
git add bashrc.d/git-worktree.sh
git commit -m "gwt: register new worktrees with project.el instead of projectile

Assisted by AI

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

### Task 5: Remove the projectile package

**Files:**
- Modify: `~/.dotfiles/init.el` — `custom-set-variables` `package-selected-packages` list (remove `projectile`)

**Interfaces:**
- Consumes: nothing.

- [ ] **Step 1: Confirm nothing references projectile anymore**

Run: `grep -n "projectile" ~/.dotfiles/init.el ~/.dotfiles/bashrc.d/git-worktree.sh`
Expected: only the entry inside `package-selected-packages` (a comment/string), no live code.

- [ ] **Step 2: Remove `projectile` from package-selected-packages**

In `~/.dotfiles/init.el`, in the `custom-set-variables` `package-selected-packages` list, delete the bare `projectile` symbol from the list (leave the rest of the list intact).

- [ ] **Step 3: Turn projectile off in the running session and autoremove**

Run:
```bash
emacsclient -e '(when (fboundp (quote projectile-mode)) (projectile-mode -1))'
```
Then, interactively in Emacs, run `M-x package-autoremove` and confirm removal of `projectile` (and any now-orphaned deps). (Autoremove is interactive; it cannot be driven headlessly here.)

- [ ] **Step 4: Verify parens + no live projectile code**

Run:
```bash
emacs --batch -Q --eval '(with-temp-buffer (insert-file-contents "~/.dotfiles/init.el") (emacs-lisp-mode) (condition-case e (progn (check-parens) (message "PARENS OK")) (error (message "PAREN ERROR: %s" e))))'
```
Expected: `PARENS OK`.

- [ ] **Step 5: Commit**

```bash
cd ~/.dotfiles
git add init.el
git commit -m "Drop projectile from selected packages

Assisted by AI

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

### Task 6: Full-restart verification

No file changes — confirms the committed config loads cleanly from scratch (the
running-session evals proved behavior, but only a fresh start proves `init.el`).

- [ ] **Step 1: Restart Emacs** (quit and relaunch, or `M-x restart-emacs` if available).

- [ ] **Step 2: Verify bindings after restart**

Run:
```bash
emacsclient -e '(list
  (cons "C-c p f" (format "%s" (key-binding (kbd "C-c p f"))))
  (cons "C-c p p" (format "%s" (key-binding (kbd "C-c p p"))))
  (cons "projectile-loaded" (featurep (quote projectile)))
  (cons "known-roots" (length (project-known-project-roots))))'
```
Expected: `project-find-file`, `project-switch-project`, `projectile-loaded` = `nil`, `known-roots` ≈ 211.

- [ ] **Step 3: Manual smoke test**

In Emacs: `C-c p p` (pick a project → lands in find-file), `C-c p f` (find a file), open a worktree file and `M-x copy-project-relative-path` (path is relative to the worktree root). Confirm all behave as before.

---

## Notes / decisions deferred to the user

- **Spec & plan commit:** these `docs/superpowers/` files are not yet committed; committing them to the public repo is the user's call (they are already sanitized of company specifics).
- **Push:** local commits only; pushing to the public remote is the user's call.
