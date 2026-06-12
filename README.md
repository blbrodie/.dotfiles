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
.bashrc                # env + PATH + aliases, then sources bashrc.d/*.sh
bashrc.d/
  git-worktree.sh      # gwt (create/switch), gwtj (from Jira), gwt-clean (prune)
  misc.sh              # small helpers (killport, curltime, ...)
  work-stuff.local.sh  # (untracked, matches *.local.sh) machine-specific helpers
```

Related functions are grouped one-file-per-topic (e.g. all three git-worktree
commands live in `git-worktree.sh`), not one file per function.

### Adding a new function

Drop a new file in `bashrc.d/`:

```bash
# bashrc.d/my-thing.sh
my_thing() { ... }
```

It's picked up automatically on the next shell start — no edit to `.bashrc`
needed. Keep each file self-contained (define functions only, no side effects)
so it stays independently sourceable and testable.

## Local / machine-specific config — keep secrets & PII out of git

**This repo is public. Never commit secrets or PII.** That means no API tokens,
passwords, or private keys, and no personal/company specifics: real email
addresses, internal hostnames or URLs, cluster names, AWS profiles, private repo
paths, etc. Committed files use generic placeholders (`you@example.com`,
`https://your-org.atlassian.net`, `<your-profile>`).

Real values live in two untracked locations (both gitignored):

- **`~/.bashrc.local`** — sourced by `.bashrc` *before* the modules load. Put
  per-machine env vars and aliases here: `JIRA_EMAIL`, `JIRA_BASE_URL`,
  `AWS_PROFILE`, project-nav aliases, etc. Functions in `bashrc.d/` read these
  via `${VAR:-placeholder}`, so they work locally and stay generic in git.
- **`bashrc.d/*.local.sh`** — gitignored function modules (still auto-loaded by
  the same `bashrc.d/*.sh` loader). Use these for entirely internal helpers.

Before committing, sanity-check the staged diff:

```bash
git grep --cached -nIiE 'your-real-domain|your-company|@your-email-domain'
```

## Tests

Shell functions are tested with [bats](https://github.com/bats-core/bats-core):

```bash
make test
```

Tests source the relevant `bashrc.d/` module directly (see
`tests/test_helper.bash`) so they run against the working tree, including inside
a git worktree.
