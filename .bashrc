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
