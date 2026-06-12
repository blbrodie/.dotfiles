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
        m=$(stat -f '%m' "$f" 2>/dev/null) || continue
        [ "$m" -gt "$newest" ] && newest="$m"
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

# Global set by _gwt_clean_load_merged_prs; consumed by _gwt_clean_pr_is_merged.
# Format: space-padded list, e.g. " feat/a feat/b ". The surrounding spaces make
# membership a single pattern match ("*\ $branch\ *") with no false positives
# from branches that are substrings of one another.
_gwt_clean_merged_prs=""

_gwt_clean_load_merged_prs() {
    # One gh round-trip to fetch every merged PR's head ref. Echoes a
    # space-padded string (leading/trailing space) so a caller can stash
    # it in $_gwt_clean_merged_prs and do membership tests with a single
    # pattern match. Replaces N serial `gh pr list --head X` calls with a
    # single call — on repos with many no-upstream worktrees this is the
    # dominant speedup.
    local list
    list=$(gh pr list --state merged --json headRefName \
            --jq '.[].headRefName' --limit 1000 2>/dev/null) || return 1
    echo " $(echo "$list" | tr '\n' ' ')"
}

_gwt_clean_pr_is_merged() {
    # Usage: _gwt_clean_pr_is_merged <branch>
    # Pure-bash lookup against the preloaded _gwt_clean_merged_prs set.
    case "$_gwt_clean_merged_prs" in
        *" $1 "*) return 0 ;;
        *) return 1 ;;
    esac
}

gwt-clean() {
    local force=0
    local stale_days=120
    local check_merged_prs=0

    while [ $# -gt 0 ]; do
        case "$1" in
            --force|-f) force=1; shift ;;
            --stale-days) stale_days="$2"; shift 2 ;;
            --check-merged-prs) check_merged_prs=1; shift ;;
            --help|-h)
                echo "Usage: gwt-clean [--force] [--stale-days N] [--check-merged-prs]"
                echo ""
                echo "  --force              Actually delete (default: dry run)"
                echo "  --stale-days N       Override 120-day stale threshold"
                echo "  --check-merged-prs   For 'no upstream' branches, query GitHub via"
                echo "                       'gh' to see if a merged PR exists with that"
                echo "                       branch as head; treat such branches as merged."
                echo "                       Requires 'gh' installed and authenticated."
                return 0
                ;;
            *)
                echo "Unknown option: $1" >&2
                echo "Usage: gwt-clean [--force] [--stale-days N] [--check-merged-prs]" >&2
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

    _gwt_clean_merged_prs=""
    if [ "$check_merged_prs" -eq 1 ]; then
        if ! command -v gh >/dev/null 2>&1; then
            echo "  (--check-merged-prs: 'gh' not found; skipping PR check)"
            check_merged_prs=0
        elif ! (cd "$git_root" && gh auth status >/dev/null 2>&1); then
            echo "  (--check-merged-prs: 'gh' not authenticated; skipping PR check)"
            check_merged_prs=0
        else
            echo "  (--check-merged-prs: fetching merged PR list from GitHub...)"
            local loaded
            if loaded=$(cd "$git_root" && _gwt_clean_load_merged_prs); then
                _gwt_clean_merged_prs="$loaded"
            else
                echo "  (--check-merged-prs: gh call failed; skipping PR check)"
                check_merged_prs=0
            fi
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
            # Opt-in: for "no upstream" branches, ask GitHub whether a
            # merged PR exists with this branch as head. Catches the case
            # of branches pushed without -u, squash-merged, and pruned.
            if [ "$check_merged_prs" -eq 1 ] && \
                    [ "$clean_reason" = "no upstream" ] && \
                    [ -n "$branch" ] && \
                    _gwt_clean_pr_is_merged "$branch"; then
                printf "%-14s %-32s %s\n" "DELETE" "$rel_name" "merged PR (no upstream)"
                to_delete_paths+=("$wt_path")
                to_delete_branches+=("$branch")
                to_delete_branch_action+=("force")
                delete_count=$((delete_count + 1))
                continue
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
