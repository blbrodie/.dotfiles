export BASH_SILENCE_DEPRECATION_WARNING=1
export PS1="\W\$ "
export CLICOLOR=1

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

# . /usr/local/opt/asdf/asdf.sh
# . /usr/local/opt/asdf/etc/bash_completion.d/asdf.bash

# Don't set if already set by emacs
export EDITOR='emacs -nw'

# build erlang with docs
export KERL_BUILD_DOCS=yes

alias tags='git ls-files | ctags --extra=+q -e -R --links=no -L-'

eval "$(/opt/homebrew/bin/brew shellenv)"

# eval $(perl -I ~/perl5/lib/perl5/ -Mlocal::lib)
function gitname {
  git config --replace-all user.name $1
}

function killport {
  lsof -i :"$1" | tail -1 | awk '{ print $2 }' | xargs kill
}

export AWS_PROFILE=whatnot_eng_user


nuke-docker() {
  docker stop $(docker ps -aq) 2>/dev/null
  docker rm $(docker ps -aq) 2>/dev/null
  docker volume rm $(docker volume ls -q) 2>/dev/null
  docker system prune -a -f
}

alias mbe="cd ~/dev/whatnot_backend/"
alias live="cd ~/dev/whatnot_live/"

# aws sso login --profile whatnot_eng_user
# aws sso login
# aws --profile whatnot_eng_user sts get-caller-identity
aws_profile() {
    grep profile ~/.aws/config  | awk '{print $2}' | tr -d ']'
    printf "Please select: "
    read aws_profile

    export AWS_PROFILE=$aws_profile
}

a_pod() {
  kubectl exec -ti $(kubectl get pods | grep -v NAME | awk '{print $1}' | fzf) -- /bin/sh
}

export PATH=$PATH:$(brew --prefix)/opt/python/libexec/bin
export PATH=$PATH:~/.emacs.d/bin

# # Nix
# if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
#   . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
# fi

# End Nix
# shellcheck source=~/.bashrc.local
source ~/.bashrc.local

eval "$(direnv hook bash)"

function curltime {
  curl -w @- -o /dev/null -s "$@" <<'EOF'
    time_namelookup:  %{time_namelookup}\n
       time_connect:  %{time_connect}\n
    time_appconnect:  %{time_appconnect}\n
   time_pretransfer:  %{time_pretransfer}\n
      time_redirect:  %{time_redirect}\n
 time_starttransfer:  %{time_starttransfer}\n
                    ----------\n
         time_total:  %{time_total}\n
EOF
}



# git worktrees
_gwt_list() {
      # Prints worktree paths relative to <repo>/worktrees/, one per line.
      # Uses `git worktree list` so branch names containing '/' work correctly.
      local git_common_dir=$(git rev-parse --git-common-dir 2>/dev/null)
      [ -z "$git_common_dir" ] && return
      local git_root=$(dirname "$git_common_dir")
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

      # add to emacs projectile
      emacsclient -e "(projectile-add-known-project \"$(pwd)/\")"
  }
  # Bash completion function
_gwt_completion() {
      local cur="${COMP_WORDS[COMP_CWORD]}"
      local worktrees=$(_gwt_list)
      COMPREPLY=($(compgen -W "$worktrees" -- "$cur"))
  }
# Register the completion
complete -F _gwt_completion gwt

# --- gwt-clean: BEGIN ---
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
    # Echoes unix timestamp of newest file anywhere in the worktree tree.
    local wt="$1"
    find "$wt" -type f -exec stat -f '%m' {} + 2>/dev/null | sort -n | tail -1
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

_gwt_clean_dir_size_kb() {
    # Echoes size of a directory in KB (integer).
    du -sk "$1" 2>/dev/null | awk '{print $1}'
}

_gwt_clean_format_kb() {
    # Usage: _gwt_clean_format_kb <kb>
    # Echoes human-readable size (K/M/G).
    local kb="$1"
    if [ "$kb" -lt 1024 ]; then
        echo "${kb}K"
    elif [ "$kb" -lt $((1024 * 1024)) ]; then
        echo "$((kb / 1024))M"
    else
        echo "$((kb / 1024 / 1024))G"
    fi
}

gwt-clean() {
    local force=0
    local stale_days=120

    while [ $# -gt 0 ]; do
        case "$1" in
            --force|-f) force=1; shift ;;
            --stale-days) stale_days="$2"; shift 2 ;;
            --help|-h)
                echo "Usage: gwt-clean [--force] [--stale-days N]"
                echo ""
                echo "  --force          Actually delete (default: dry run)"
                echo "  --stale-days N   Override 120-day stale threshold"
                return 0
                ;;
            *)
                echo "Unknown option: $1" >&2
                echo "Usage: gwt-clean [--force] [--stale-days N]" >&2
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

    local -a to_delete_paths=()
    local -a to_delete_branches=()
    local -a to_delete_merged=()
    local total_count=0 delete_count=0 reclaim_kb=0

    while IFS= read -r wt_path; do
        [ -z "$wt_path" ] && continue
        total_count=$((total_count + 1))
        local size_kb
        size_kb=$(_gwt_clean_dir_size_kb "$wt_path")
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
            printf "%-14s %-32s %s\n" "KEEP: dirty" "$rel_name" "$clean_reason"
            continue
        fi

        local merged=0 stale=0
        (cd "$git_root" && _gwt_clean_is_merged "$branch" "$default_branch") && merged=1
        _gwt_clean_is_stale "$wt_path" "$stale_days" && stale=1

        local age; age=$(_gwt_clean_age_days "$wt_path")
        if [ "$merged" -eq 1 ] || [ "$stale" -eq 1 ]; then
            local reason=""
            [ "$merged" -eq 1 ] && reason="merged"
            [ "$stale" -eq 1 ] && reason="${reason:+${reason}, }stale (${age}d)"
            reason="${reason}, clean"
            printf "%-14s %-32s %s\n" "DELETE" "$rel_name" "$reason"
            to_delete_paths+=("$wt_path")
            to_delete_branches+=("$branch")
            to_delete_merged+=("$merged")
            delete_count=$((delete_count + 1))
            reclaim_kb=$((reclaim_kb + size_kb))
        else
            printf "%-14s %-32s %s\n" "KEEP: active" "$rel_name" "clean, ${age}d old, unmerged"
        fi
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
    local reclaim_human; reclaim_human=$(_gwt_clean_format_kb "$reclaim_kb")
    echo "$total_count worktrees: $delete_count will be deleted, $((total_count - delete_count)) will be kept"
    echo "Reclaimable: ~${reclaim_human}"

    if [ "$force" -eq 0 ]; then
        echo ""
        echo "(dry run — pass --force to delete)"
        return 0
    fi

    echo ""
    echo "Deleting..."
    local deleted=0 freed_kb=0 i=0
    while [ "$i" -lt "${#to_delete_paths[@]}" ]; do
        local wt="${to_delete_paths[$i]}"
        local br="${to_delete_branches[$i]}"
        local mg="${to_delete_merged[$i]}"
        local sz; sz=$(_gwt_clean_dir_size_kb "$wt")
        if (cd "$git_root" && git worktree remove "$wt" 2>/dev/null); then
            deleted=$((deleted + 1))
            freed_kb=$((freed_kb + sz))
            if [ "$mg" = "1" ] && [ -n "$br" ]; then
                (cd "$git_root" && git branch -d "$br" 2>/dev/null) || \
                    echo "  (kept branch $br: not fully merged locally)"
            fi
        else
            echo "  Warning: failed to remove $wt; skipping"
        fi
        i=$((i + 1))
    done
    (cd "$git_root" && git worktree prune 2>/dev/null)
    echo ""
    local freed_human; freed_human=$(_gwt_clean_format_kb "$freed_kb")
    echo "Deleted $deleted worktrees, freed ~${freed_human}"
}
# --- gwt-clean: END ---

export MYPY="mypy --skip-cache-mtime-checks --exclude worktrees"


# ---------------------------------------
# Whatnot MBE Access

# slab: https://whatnot.slab.com/posts/accessing-infrastructure-locally-ifh1gde0#h6e5m-quick-start-to-access-mbe-in-prod

# 1. Login to Twingate on local machine
# 2. Request access to StrongDM via /appstore in Slack (SDM - Engineering Base)
# 3. Request access to Amazon Web Services via /appstore in Slack (Read/Write (Prod/Stage Envs))
# 4. Open StrongDM, authenticate via Okta
mbe-prod() {
    sdm connect prod1-use1-eng-admin
    sdm kubernetes update-config prod1-use1-eng-admin
    kubectl exec -i -t $(kubectl get pods -n main-backend | grep -v "main-backend-gql" | grep main-backend | cut -d " " -f1 | head -1) -n main-backend -- bash
}

mbe-stage() {
    sdm connect stage2-use1-eng-user
    sdm kubernetes update-config stage2-use1-eng-user
    kubectl exec -i -t $(kubectl get pods -n main-backend | grep -v "main-backend-gql" | grep main-backend | cut -d " " -f1 | head -1) -n main-backend -- bash
}
# ---------------------------------------

export PATH=~/dev/whatnot/scripts:$PATH

vterm_printf() {
    if [ -n "$TMUX" ] \
        && { [ "${TERM%%-*}" = "tmux" ] \
            || [ "${TERM%%-*}" = "screen" ]; }; then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}


# uv
export PATH="/Users/ben/.local/bin:$PATH"

. "$HOME/.local/bin/env"

alias claude='claude --model opus'
