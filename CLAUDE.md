# Working in this repo

Personal macOS dotfiles. See `README.md` for the full layout; this file is the
short version of the rules to follow when making changes.

## This repo is PUBLIC — never commit secrets or PII

No tokens, passwords, or private keys. No personal/company specifics either:
real emails, internal hostnames/URLs, cluster names, AWS profiles, private repo
paths, etc. Tracked files use generic placeholders (`you@example.com`,
`https://your-org.atlassian.net`, `<your-profile>`).

Real values belong in untracked, gitignored locations:
- `~/.bashrc.local` — per-machine env vars and aliases (sourced before modules).
- `bashrc.d/*.local.sh` — internal-only function modules (still auto-loaded).

Functions reference real values via `${VAR:-placeholder}` so they stay generic
in git. Before committing, scan the staged diff for anything sensitive.

## Shell functions

- Live in `bashrc.d/`, one file per topic (not per function). `.bashrc`
  auto-sources every `bashrc.d/*.sh` on startup — no `.bashrc` edit needed.
- Keep each file self-contained: define functions only, no side effects at
  source time, so it stays independently sourceable and testable.

## Tests (TDD)

- Write a failing `bats` test first, then implement. Tests are in `tests/`.
- A test sources its module via the `source_*` helpers in
  `tests/test_helper.bash`, which read from the working tree (so they pass
  inside a git worktree before changes are merged).
- Run the suite with `make test`. Keep it green before committing.
