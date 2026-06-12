# git worktree helper: gwt (create/switch worktrees) + bash completion.

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
