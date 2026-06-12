# dotfiles

Personal macOS dotfiles: Bash, Emacs, tmux, ctags, and a Homebrew bundle.

## Install

```bash
cd ~ && git clone git@github.com:blbrodie/.dotfiles.git && cd .dotfiles && make
```

`make` (the default `all` target) installs the Homebrew bundle and symlinks the
config files into `$HOME`:

| Target  | What it does                                              |
|---------|-----------------------------------------------------------|
| `make`  | `brew emacs bash tmux` (everything below)                 |
| `brew`  | `brew bundle` from the `Brewfile`                          |
| `emacs` | symlink `init.el` → `~/.emacs.d/init.el`                   |
| `bash`  | symlink `.bash_profile` and `.bashrc` → `~`               |
| `tmux`  | symlink `.tmux.conf` → `~/.tmux.conf`                      |
| `test`  | run the shell-function test suite (`bats tests/`)         |

## Shell layout

`.bashrc` stays small: environment variables, `PATH`, and aliases. All shell
**functions** live in `bashrc.d/`, and `.bashrc` ends with a loader that sources
every `bashrc.d/*.sh` file on startup:

```
.bashrc                      # env + PATH + aliases, then sources bashrc.d/*.sh
bashrc.d/
  git-worktree.sh            # gwt        — create/switch git worktrees
  git-worktree-jira.sh       # gwtj       — create a worktree from a Jira issue
  git-worktree-clean.sh      # gwt-clean  — prune merged/stale worktrees
  misc.sh                    # small helpers (killport, curltime, ...)
  work-stuff.local.sh        # (untracked, *.local.sh) machine-specific helpers
```

### Adding a new function

Drop a new file in `bashrc.d/`:

```bash
# bashrc.d/my-thing.sh
my_thing() { ... }
```

It's picked up automatically on the next shell start — no edit to `.bashrc`
needed. Keep each file self-contained (define functions only, no side effects)
so it stays independently sourceable and testable.

## Local / machine-specific config

Anything personal, company-specific, or secret stays **out of this repo**:

- `~/.bashrc.local` — sourced by `.bashrc` before the modules load. Put env
  vars like `JIRA_EMAIL`, `JIRA_BASE_URL`, and `AWS_PROFILE` here.
- `bashrc.d/*.local.sh` — gitignored function modules (still auto-loaded).

The committed files use generic placeholders (`you@example.com`,
`https://your-org.atlassian.net`); your real values come from `~/.bashrc.local`.

## Tests

Shell functions are tested with [bats](https://github.com/bats-core/bats-core):

```bash
make test
```

Tests source the relevant `bashrc.d/` module directly (see
`tests/test_helper.bash`) so they run against the working tree, including inside
a git worktree.
