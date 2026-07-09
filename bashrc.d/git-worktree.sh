# Git worktree helpers. Worktrees live under <repo>/worktrees/<branch>.
#
#   gwt        create or switch to a worktree (new branches fork origin/main)
#   gwtj       create a worktree from a Jira issue (branch named from the ticket)
#   gwt-clean  prune merged / stale worktrees
#   gwt-rm     force-remove named worktrees (escape hatch for never-PR'd work)

# ===== gwt: create / switch worktrees =====

# git worktree helper: gwt (create/switch worktrees) + bash completion.

# git worktrees
_gwt_list() {
      # Prints worktree paths relative to <repo>/worktrees/, one per line.
      # Uses `git worktree list` so branch names containing '/' work correctly.
      local git_common_dir=$(git rev-parse --git-common-dir 2>/dev/null)
      [ -z "$git_common_dir" ] && return
      # Resolve to an absolute physical path: at the repo root --git-common-dir
      # is the relative ".git", but `git worktree list` prints absolute paths,
      # so a relative prefix would never match and the list would come back empty.
      local git_root=$(cd "$(dirname "$git_common_dir")" && pwd -P) || return
      local prefix="$git_root/worktrees/"
      git worktree list --porcelain 2>/dev/null | awk -v prefix="$prefix" '
          /^worktree / {
              path = substr($0, 10)
              if (substr(path, 1, length(prefix)) == prefix) {
                  print substr(path, length(prefix) + 1)
              }
          }
      '
}
gwt() {
      if [ -z "$1" ]; then
          echo "Usage: gwt <branch_name>"
          echo "Available worktrees:"
          local wts=$(_gwt_list)
          if [ -n "$wts" ]; then
              echo "$wts" | sed 's/^/  /'
          else
              echo "  (no worktrees found)"
          fi
          return 1
      fi

      local git_common_dir=$(git rev-parse --git-common-dir 2>/dev/null)
      if [ -z "$git_common_dir" ]; then
          echo "Error: Not in a git repository"
          return 1
      fi

      local git_root=$(dirname "$git_common_dir")
      cd "$git_root"

      # Check if worktree already exists at the expected path
      if [ -d "worktrees/$1" ]; then
          echo "Worktree '$1' already exists, changing to it..."
          cd "worktrees/$1"
          return 0
      fi

      # Check if the branch is already checked out in *some other* worktree
      local existing_worktree=$(git worktree list --porcelain 2>/dev/null | awk -v branch="refs/heads/$1" '
          /^worktree / { path = substr($0, 10) }
          $0 == "branch " branch { print path; exit }
      ')
      if [ -n "$existing_worktree" ]; then
          echo "Branch '$1' is already checked out at $existing_worktree, changing to it..."
          cd "$existing_worktree" || return 1
          return 0
      fi

      # Create new worktree
      git fetch
      if git show-ref --verify --quiet "refs/heads/$1"; then
          # Local branch exists, use it
          git worktree add "worktrees/$1" "$1" || return 1
      elif git show-ref --verify --quiet "refs/remotes/origin/$1"; then
          # Remote branch exists, create tracking branch
          git worktree add "worktrees/$1" --track -b "$1" "origin/$1" || return 1
      else
          # Create new branch from origin/main
          git worktree add "worktrees/$1" -b "$1" origin/main || return 1
      fi
      if [ -e ".envrc" ]; then
        cp .envrc worktrees/$1/.envrc
        direnv allow "worktrees/$1"
      fi
      if [ -e ".env" ]; then
        cp .env worktrees/$1/.env
      fi
      cd "worktrees/$1"

      # register the new worktree with Emacs project.el (instant C-c p p switch)
      emacsclient -e "(project-remember-project (project-current nil \"$(pwd)/\"))" >/dev/null 2>&1
  }
  # Bash completion function
_gwt_completion() {
      local cur="${COMP_WORDS[COMP_CWORD]}"
      local worktrees=$(_gwt_list)
      COMPREPLY=($(compgen -W "$worktrees" -- "$cur"))
  }
# Register the completion (guarded: `complete` is unavailable in some
# non-interactive shells, and sourcing this module must never hard-fail).
complete -F _gwt_completion gwt 2>/dev/null || true

# ===== gwtj: create a worktree from a Jira issue =====

# gwtj: create a git worktree from a Jira issue.
# Branch name = <KEY>-<slugified-summary>, e.g. PROJ-1234-fix-login-redirect.
# Credentials come from env; set JIRA_BASE_URL and JIRA_EMAIL in ~/.bashrc.local.
# If JIRA_API_TOKEN is unset, the token is read from the macOS keychain.
# One-time setup (create a token at id.atlassian.com -> Security -> API tokens):
#   security add-generic-password -s jira-api-token -a "$JIRA_EMAIL" -w
JIRA_BASE_URL="${JIRA_BASE_URL:-https://your-org.atlassian.net}"
JIRA_EMAIL="${JIRA_EMAIL:-you@example.com}"

_gwtj_key() {
    # Extract an ISSUE-123 key from a Jira URL or bare key, normalized to uppercase.
    printf '%s' "$1" | grep -oiE '[A-Z][A-Z0-9]+-[0-9]+' | head -n1 | tr '[:lower:]' '[:upper:]'
}

_gwtj_slug() {
    # Slugify stdin: lowercase, non-alnum -> '-', collapse/trim.
    # If longer than 50 chars, truncate at a word ('-') boundary.
    local slug
    slug=$(tr '[:upper:]' '[:lower:]' | sed -E 's/[^a-z0-9]+/-/g; s/^-+//; s/-+$//')
    if [ "${#slug}" -le 50 ]; then
        printf '%s' "$slug"
        return
    fi
    local head="${slug:0:50}"
    # Drop a trailing partial word unless char 51 already starts a new word.
    [ "${slug:50:1}" != "-" ] && head="${head%-*}"
    head="${head%-}"
    # Fallback: a single word longer than 50 chars has no boundary to cut on.
    [ -z "$head" ] && head="${slug:0:50}"
    printf '%s' "$head"
}

_gwtj_token() {
    # Prefer JIRA_API_TOKEN; fall back to the macOS keychain.
    if [ -n "${JIRA_API_TOKEN:-}" ]; then
        printf '%s' "$JIRA_API_TOKEN"
        return 0
    fi
    security find-generic-password -s jira-api-token -a "$JIRA_EMAIL" -w 2>/dev/null
}

_gwtj_context_body() {
    # Render CLAUDE.local.md body. Args: <key> <status> <summary>.
    local key="$1" status="$2" summary="$3"
    printf '# Jira: %s\n' "$key"
    printf '%s/browse/%s\n' "$JIRA_BASE_URL" "$key"
    [ -n "$status" ] && printf 'Status: %s\n' "$status"
    [ -n "$summary" ] && printf 'Summary: %s\n' "$summary"
    printf '\nFor the full description, comments, and live status, fetch this ticket via the Atlassian MCP plugin (or open the link above).\n'
}

gwtj() {
    if [ -z "$1" ]; then
        echo "Usage: gwtj <jira-url-or-key>"
        return 1
    fi

    local key
    key=$(_gwtj_key "$1")
    if [ -z "$key" ]; then
        echo "Error: could not find a Jira issue key in '$1'" >&2
        return 1
    fi

    local token
    token=$(_gwtj_token)
    if [ -z "$token" ]; then
        echo "Error: no Jira API token found." >&2
        echo "Set JIRA_API_TOKEN, or store one in the keychain:" >&2
        echo "  security add-generic-password -s jira-api-token -a \"$JIRA_EMAIL\" -w" >&2
        return 1
    fi

    local resp summary status branch="$key"
    resp=$(curl -sf -u "$JIRA_EMAIL:$token" -H "Accept: application/json" \
        "$JIRA_BASE_URL/rest/api/3/issue/$key?fields=summary,status")
    summary=$(printf '%s' "$resp" | jq -r '(.fields.summary // "") | gsub("^\\s+|\\s+$";"")' 2>/dev/null)
    status=$(printf '%s' "$resp" | jq -r '(.fields.status.name // "") | gsub("^\\s+|\\s+$";"")' 2>/dev/null)

    if [ -n "$summary" ]; then
        local slug
        slug=$(printf '%s' "$summary" | _gwtj_slug)
        [ -n "$slug" ] && branch="$key-$slug"
    else
        echo "Warning: couldn't fetch summary for $key; using key only" >&2
    fi

    echo "Creating worktree: $branch"
    gwt "$branch" || return 1

    # gwt leaves us in the worktree root on success. Drop a Jira context file
    # that Claude Code auto-loads, and exclude it locally so it's never committed.
    if [ ! -e CLAUDE.local.md ]; then
        _gwtj_context_body "$key" "$status" "$summary" > CLAUDE.local.md
        local exclude current=""
        exclude="$(git rev-parse --git-common-dir 2>/dev/null)/info/exclude"
        [ -f "$exclude" ] && current=$(<"$exclude")
        case $'\n'"$current"$'\n' in
            *$'\n'CLAUDE.local.md$'\n'*) ;;  # already excluded
            *) [ -d "$(dirname "$exclude")" ] && printf '%s\n' 'CLAUDE.local.md' >> "$exclude" ;;
        esac
        echo "Wrote CLAUDE.local.md with $key context"
    fi
}

# ===== gwt-clean: prune merged / stale worktrees =====

_gwt_clean_default_branch() {
    # Echo "main" or "master" (whichever exists locally), or empty if neither.
    if git show-ref --verify --quiet refs/heads/main 2>/dev/null; then
        echo "main"
    elif git show-ref --verify --quiet refs/heads/master 2>/dev/null; then
        echo "master"
    fi
}
_gwt_clean_is_merged() {
    # Usage: _gwt_clean_is_merged <branch> <default_branch>
    # Returns 0 if branch is reachable from default_branch OR has [gone] upstream.
    # Caller is responsible for having run `git fetch --prune` beforehand.
    local branch="$1" default_branch="$2"
    if [ -n "$default_branch" ]; then
        if git branch --merged "$default_branch" 2>/dev/null | \
                sed 's/^[ *+]*//' | grep -qxF "$branch"; then
            return 0
        fi
    fi
    local upstream_status
    upstream_status=$(git for-each-ref --format='%(upstream:track)' "refs/heads/$branch" 2>/dev/null)
    [ "$upstream_status" = "[gone]" ]
}
_gwt_clean_is_clean() {
    # Usage: _gwt_clean_is_clean <worktree-path>
    # Returns 0 if clean. Returns 1 and echoes reason if not.
    local wt="$1"
    if [ -n "$(git -C "$wt" status --porcelain 2>/dev/null)" ]; then
        echo "uncommitted changes"
        return 1
    fi
    if ! git -C "$wt" rev-parse --abbrev-ref --symbolic-full-name @{u} >/dev/null 2>&1; then
        echo "no upstream"
        return 1
    fi
    local unpushed
    unpushed=$(git -C "$wt" rev-list --count @{u}..HEAD 2>/dev/null)
    if [ -n "$unpushed" ] && [ "$unpushed" != "0" ]; then
        echo "unpushed commits ($unpushed)"
        return 1
    fi
    return 0
}
_gwt_clean_newest_mtime() {
    # Echoes a unix timestamp reflecting recent ref-changing activity on
    # this worktree. Reads HEAD and logs/HEAD (the reflog) in the gitdir
    # rather than scanning every file — for a clean worktree (the only
    # case staleness matters), ref-changing operations (checkout, commit,
    # reset, pull, merge, rebase) all update at least one of these.
    # .git/index is intentionally excluded because `git status` refreshes
    # its stat cache on every invocation, which would make any worktree
    # we've just classified look "active".
    local wt="$1"
    local gitdir
    gitdir=$(git -C "$wt" rev-parse --git-dir 2>/dev/null) || return
    [ "${gitdir:0:1}" != "/" ] && gitdir="$wt/$gitdir"
    local newest=0 f m
    for f in "$gitdir/HEAD" "$gitdir/logs/HEAD"; do
        [ -f "$f" ] || continue
        # GNU (Linux) stat uses -c '%Y'; BSD (macOS) stat uses -f '%m'.
        # GNU-first is deliberate: BSD's `-c` fails cleanly (usage to
        # stderr, no stdout), whereas GNU's `-f` prints filesystem info to
        # stdout, which would corrupt the value if tried first.
        m=$(stat -c '%Y' "$f" 2>/dev/null || stat -f '%m' "$f" 2>/dev/null) || continue
        [ -n "$m" ] && [ "$m" -gt "$newest" ] && newest="$m"
    done
    [ "$newest" -gt 0 ] && echo "$newest"
}

_gwt_clean_is_stale() {
    # Usage: _gwt_clean_is_stale <worktree-path> <stale_days>
    # Returns 0 if newest file mtime is older than stale_days.
    local wt="$1" stale_days="$2"
    local newest
    newest=$(_gwt_clean_newest_mtime "$wt")
    [ -z "$newest" ] && return 0  # empty worktree: treat as stale
    local threshold=$(( $(date +%s) - stale_days * 86400 ))
    [ "$newest" -lt "$threshold" ]
}

_gwt_clean_age_days() {
    # Echoes integer age in days of newest file mtime.
    local wt="$1"
    local newest
    newest=$(_gwt_clean_newest_mtime "$wt")
    [ -z "$newest" ] && { echo 9999; return; }
    echo $(( ($(date +%s) - newest) / 86400 ))
}

_gwt_clean_commit_age_days() {
    # Echoes integer age in days of HEAD's commit date. Unlike file mtimes,
    # commit date is not perturbed by gc / fetch / worktree maintenance
    # (which can bump reflog mtimes long after the last real work), so it is
    # the reliable "recent work" signal for the merged-PR override: a
    # post-merge follow-up is a commit, and updates this.
    local wt="$1" ct
    ct=$(git -C "$wt" log -1 --format=%ct HEAD 2>/dev/null)
    [ -z "$ct" ] && { echo 9999; return; }
    echo $(( ($(date +%s) - ct) / 86400 ))
}

_gwt_clean_pr_state() {
    # Usage: _gwt_clean_pr_state <branch>
    # Echoes the PR state for this branch head: MERGED, OPEN, CLOSED, or
    # NONE (no PR / gh error). Prefers MERGED > OPEN > CLOSED when a head
    # has had multiple PRs.
    #
    # This is a per-branch query, NOT a bulk `gh pr list --state merged`
    # preload. On a high-volume repo the most-recent-N merged PRs cover
    # only days, so a bulk list silently misses branches merged earlier
    # and reports them as unmerged. Callers gate this behind cheap local
    # filters (dirty-but-not-uncommitted + stale) so the round-trip is
    # paid only for real deletion candidates.
    local branch="$1"
    [ -n "$branch" ] || { echo "NONE"; return; }
    local out
    out=$(gh pr list --head "$branch" --state all --json state \
            --jq '[.[].state] as $s
                  | if   ($s | index("MERGED")) then "MERGED"
                    elif ($s | index("OPEN"))   then "OPEN"
                    elif ($s | index("CLOSED")) then "CLOSED"
                    else "NONE" end' 2>/dev/null)
    echo "${out:-NONE}"
}

gwt-clean() {
    local force=0
    local stale_days=120
    local check_merged_prs=0
    local include_closed=0

    while [ $# -gt 0 ]; do
        case "$1" in
            --force|-f) force=1; shift ;;
            --stale-days) stale_days="$2"; shift 2 ;;
            --check-merged-prs) check_merged_prs=1; shift ;;
            --include-closed) check_merged_prs=1; include_closed=1; shift ;;
            --help|-h)
                echo "Usage: gwt-clean [--force] [--stale-days N] [--check-merged-prs] [--include-closed]"
                echo ""
                echo "  --force              Actually delete (default: dry run)"
                echo "  --stale-days N       Override 120-day stale threshold"
                echo "  --check-merged-prs   For a dirty worktree whose only 'dirt' is a"
                echo "                       missing upstream or unpushed commits (NOT"
                echo "                       uncommitted edits), query GitHub via 'gh' for a"
                echo "                       merged PR on that branch head. If merged AND the"
                echo "                       worktree is stale, delete it: the reviewed work"
                echo "                       is in the default branch and no recent follow-up"
                echo "                       remains. Requires 'gh' installed and authed."
                echo "  --include-closed     Also delete such worktrees whose PR was CLOSED"
                echo "                       without merging (rejected/abandoned work)."
                echo "                       Implies --check-merged-prs."
                return 0
                ;;
            *)
                echo "Unknown option: $1" >&2
                echo "Usage: gwt-clean [--force] [--stale-days N] [--check-merged-prs] [--include-closed]" >&2
                return 2
                ;;
        esac
    done

    local git_common_dir
    git_common_dir=$(git rev-parse --git-common-dir 2>/dev/null)
    if [ -z "$git_common_dir" ]; then
        echo "Error: Not in a git repository" >&2
        return 1
    fi
    local git_root
    git_root=$(cd "$(dirname "$git_common_dir")" && pwd -P)
    local worktrees_dir="$git_root/worktrees"

    if [ ! -d "$worktrees_dir" ]; then
        echo "No worktrees to clean"
        return 0
    fi

    local current_wt
    current_wt=$(git rev-parse --show-toplevel 2>/dev/null)

    echo "Fetching and pruning remote refs..."
    (cd "$git_root" && git fetch --prune 2>/dev/null) || \
        echo "  (fetch failed; continuing with local info)"

    local default_branch
    default_branch=$(cd "$git_root" && _gwt_clean_default_branch)
    if [ -z "$default_branch" ]; then
        echo "  (no 'main' or 'master' branch locally; using [gone] check only)"
    fi

    if [ "$check_merged_prs" -eq 1 ]; then
        if ! command -v gh >/dev/null 2>&1; then
            echo "  (PR checks: 'gh' not found; skipping)"
            check_merged_prs=0; include_closed=0
        elif ! (cd "$git_root" && gh auth status >/dev/null 2>&1); then
            echo "  (PR checks: 'gh' not authenticated; skipping)"
            check_merged_prs=0; include_closed=0
        fi
    fi

    local -a to_delete_paths=()
    local -a to_delete_branches=()
    # Parallel array of branch-cleanup actions per delete candidate:
    #   "none"  — don't touch the local branch (stale/unmerged case)
    #   "safe"  — `git branch -d`; keep branch (and warn) if git refuses
    #   "force" — `git branch -D`; we have strong external evidence
    #             (merged PR confirmed via gh) that the work is in master,
    #             typically via squash-merge where -d would refuse.
    local -a to_delete_branch_action=()
    local total_count=0 delete_count=0

    while IFS= read -r wt_path; do
        [ -z "$wt_path" ] && continue
        total_count=$((total_count + 1))
        local rel_name="${wt_path#$worktrees_dir/}"
        local branch
        branch=$(git -C "$wt_path" symbolic-ref --short HEAD 2>/dev/null)

        if [ "$wt_path" = "$current_wt" ]; then
            printf "%-14s %-32s %s\n" "KEEP: current" "$rel_name" "you are here"
            continue
        fi

        local clean_reason
        clean_reason=$(_gwt_clean_is_clean "$wt_path")
        local clean_rc=$?
        if [ "$clean_rc" -ne 0 ]; then
            # Opt-in PR override for dirty worktrees. A merged PR proves the
            # branch's reviewed content is in the default branch — but says
            # nothing about a dirty delta on top, and squash-merge makes that
            # delta indistinguishable from real new work via local refs
            # alone. So we override "dirty" only when BOTH extra guards hold:
            #   1. the dirt is NOT uncommitted edits — only a missing upstream
            #      or unpushed commits (the squash-merge artifact). Reflog-
            #      based staleness can't see the working tree, so uncommitted
            #      edits are never auto-deleted.
            #   2. HEAD's commit is older than stale_days. A recent post-merge
            #      follow-up commit bumps the commit date and keeps it alive.
            #      (Commit date, not file mtime: gc/fetch bump reflog mtimes
            #      long after the last real work, which would defeat this.)
            # --include-closed additionally sweeps CLOSED (rejected) PRs.
            local commit_age; commit_age=$(_gwt_clean_commit_age_days "$wt_path")
            if [ "$check_merged_prs" -eq 1 ] && \
                    [ "$clean_reason" != "uncommitted changes" ] && \
                    [ -n "$branch" ] && \
                    [ "$commit_age" -ge "$stale_days" ]; then
                local pr_state
                pr_state=$(cd "$git_root" && _gwt_clean_pr_state "$branch")
                local age="$commit_age"
                if [ "$pr_state" = "MERGED" ] || \
                        { [ "$include_closed" -eq 1 ] && [ "$pr_state" = "CLOSED" ]; }; then
                    local why="merged PR"
                    [ "$pr_state" = "CLOSED" ] && why="closed PR"
                    printf "%-14s %-32s %s\n" "DELETE" "$rel_name" \
                        "$why, last commit ${age}d ago, $clean_reason"
                    to_delete_paths+=("$wt_path")
                    to_delete_branches+=("$branch")
                    to_delete_branch_action+=("force")
                    delete_count=$((delete_count + 1))
                    continue
                fi
            fi
            printf "%-14s %-32s %s\n" "KEEP: dirty" "$rel_name" "$clean_reason"
            continue
        fi

        # Merged check is cheap (reads refs). If merged, we're deleting
        # regardless of age, so skip the staleness check entirely.
        if (cd "$git_root" && _gwt_clean_is_merged "$branch" "$default_branch"); then
            printf "%-14s %-32s %s\n" "DELETE" "$rel_name" "merged, clean"
            to_delete_paths+=("$wt_path")
            to_delete_branches+=("$branch")
            to_delete_branch_action+=("safe")
            delete_count=$((delete_count + 1))
            continue
        fi

        local age; age=$(_gwt_clean_age_days "$wt_path")
        if _gwt_clean_is_stale "$wt_path" "$stale_days"; then
            printf "%-14s %-32s %s\n" "DELETE" "$rel_name" "stale (${age}d), clean"
            to_delete_paths+=("$wt_path")
            to_delete_branches+=("$branch")
            to_delete_branch_action+=("none")
            delete_count=$((delete_count + 1))
            continue
        fi

        printf "%-14s %-32s %s\n" "KEEP: active" "$rel_name" "clean, ${age}d old, unmerged"
    done < <(
        (cd "$git_root" && git worktree list --porcelain 2>/dev/null) |
        awk -v prefix="$worktrees_dir/" '
            /^worktree / {
                path = substr($0, 10)
                if (substr(path, 1, length(prefix)) == prefix) print path
            }
        '
    )

    echo ""
    if [ "$delete_count" -eq 0 ]; then
        echo "$total_count worktrees: 0 will be deleted, $total_count will be kept"
        echo "Nothing to do."
        return 0
    fi
    echo "$total_count worktrees: $delete_count will be deleted, $((total_count - delete_count)) will be kept"

    if [ "$force" -eq 0 ]; then
        echo ""
        echo "(dry run — pass --force to delete)"
        return 0
    fi

    echo ""
    echo "Deleting..."
    local deleted=0 i=0
    while [ "$i" -lt "${#to_delete_paths[@]}" ]; do
        local wt="${to_delete_paths[$i]}"
        local br="${to_delete_branches[$i]}"
        local action="${to_delete_branch_action[$i]}"
        if (cd "$git_root" && git worktree remove "$wt" 2>/dev/null); then
            deleted=$((deleted + 1))
            if [ -n "$br" ]; then
                case "$action" in
                    force)
                        (cd "$git_root" && git branch -D "$br" >/dev/null 2>&1) || \
                            echo "  (failed to delete branch $br)"
                        ;;
                    safe)
                        (cd "$git_root" && git branch -d "$br" 2>/dev/null) || \
                            echo "  (kept branch $br: not fully merged locally)"
                        ;;
                    none) : ;;
                esac
            fi
        else
            echo "  Warning: failed to remove $wt; skipping"
        fi
        i=$((i + 1))
    done
    (cd "$git_root" && git worktree prune 2>/dev/null)
    echo ""
    echo "Deleted $deleted worktrees"
}

# ===== gwt-rm: explicit removal of named worktrees =====

# Force-remove one or more worktrees (by name relative to <repo>/worktrees/)
# and delete their local branches — no dirty/merge checks. This is the
# escape hatch for work gwt-clean will never reclaim on its own: branches
# that never had a PR, where the worktree holds the only copy of the work.
# There is no safe automatic signal that such work is disposable, so
# discarding it is a deliberate, per-name decision.
gwt-rm() {
    if [ -z "$1" ]; then
        echo "Usage: gwt-rm <name> [name...]" >&2
        echo "  Force-removes worktrees under <repo>/worktrees/ and deletes" >&2
        echo "  their local branches. Discards uncommitted changes and unmerged" >&2
        echo "  commits without asking — intended for branches you've decided to" >&2
        echo "  drop that gwt-clean leaves alone (e.g. never had a PR)." >&2
        return 2
    fi

    local git_common_dir
    git_common_dir=$(git rev-parse --git-common-dir 2>/dev/null)
    if [ -z "$git_common_dir" ]; then
        echo "Error: Not in a git repository" >&2
        return 1
    fi
    local git_root
    git_root=$(cd "$(dirname "$git_common_dir")" && pwd -P)
    local worktrees_dir="$git_root/worktrees"

    local name wt branch removed=0
    for name in "$@"; do
        wt="$worktrees_dir/$name"
        if [ ! -d "$wt" ]; then
            echo "  skip: no worktree at '$name'" >&2
            continue
        fi
        branch=$(git -C "$wt" symbolic-ref --short HEAD 2>/dev/null)
        if (cd "$git_root" && git worktree remove --force "$wt" 2>/dev/null); then
            removed=$((removed + 1))
            echo "removed $name"
            if [ -n "$branch" ]; then
                (cd "$git_root" && git branch -D "$branch" >/dev/null 2>&1) && \
                    echo "  deleted branch $branch"
            fi
        else
            echo "  failed to remove '$name'" >&2
        fi
    done
    (cd "$git_root" && git worktree prune 2>/dev/null)
    echo "Removed $removed worktrees"
}
complete -F _gwt_completion gwt-rm 2>/dev/null || true
