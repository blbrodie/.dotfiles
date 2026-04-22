# ~/.dotfiles/tests/test_helper.bash
# Shared fixtures for gwt-clean tests.

# Source only the gwt-clean section of .bashrc (between markers).
# This avoids executing the rest of .bashrc (exports, aliases, PATH tweaks).
source_gwt_clean() {
    # Source the gwt-clean section from the .bashrc sitting at the root of
    # this repo/worktree (one level above tests/), NOT the user's installed
    # $HOME/.dotfiles/.bashrc. This lets tests run correctly inside a git
    # worktree where changes haven't been merged to master yet.
    local rc="${BATS_TEST_DIRNAME}/../.bashrc"
    source <(awk '/^# --- gwt-clean: BEGIN ---/,/^# --- gwt-clean: END ---/' "$rc")
}

# Create a fresh git repo with a bare remote, a main branch, and one commit.
# Echoes the repo path.
create_test_repo() {
    local dir
    dir=$(mktemp -d -t gwtclean)
    git init -q -b main "$dir"
    git -C "$dir" config user.email "test@test.com"
    git -C "$dir" config user.name "Test"
    echo "initial" > "$dir/README.md"
    git -C "$dir" add README.md
    git -C "$dir" commit -qm "initial"
    local remote="${dir}.remote"
    git init -q --bare "$remote"
    git -C "$dir" remote add origin "$remote"
    git -C "$dir" push -q -u origin main
    echo "$dir"
}

# Remove a test repo and its bare remote.
cleanup_test_repo() {
    local dir="$1"
    [ -n "$dir" ] && rm -rf "$dir" "${dir}.remote"
}

# Create a worktree for a new branch, add an initial commit, and push to origin by default.
# Usage: create_worktree <repo> <branch> [--no-push]
create_worktree() {
    local repo="$1" branch="$2" push_flag="${3:-push}"
    git -C "$repo" worktree add -q "$repo/worktrees/$branch" -b "$branch" >/dev/null
    # Add an initial commit so the branch has work of its own (needed for
    # merge-detection logic to distinguish "merged" from "just branched").
    echo "# $branch" > "$repo/worktrees/$branch/.branch"
    git -C "$repo/worktrees/$branch" add .branch
    git -C "$repo/worktrees/$branch" commit -qm "init $branch"
    if [ "$push_flag" = "push" ]; then
        git -C "$repo/worktrees/$branch" push -q -u origin "$branch"
    fi
}

# Merge a branch into main (fast-forward or merge commit) and push.
merge_branch_to_main() {
    local repo="$1" branch="$2"
    git -C "$repo" checkout -q main
    git -C "$repo" merge -q --no-ff "$branch" -m "merge $branch"
    git -C "$repo" push -q origin main
    git -C "$repo" checkout -q -
}

# Delete a branch on the remote, then fetch --prune locally so upstream goes [gone].
delete_remote_branch() {
    local repo="$1" branch="$2"
    git -C "$repo" push -q origin --delete "$branch"
    git -C "$repo" fetch -q --prune origin
}

# Set every file under <path> to an mtime of N days ago.
set_path_age_days() {
    local path="$1" days="$2"
    local ts
    ts=$(date -v-"${days}"d +%Y%m%d%H%M.%S)
    find "$path" -exec touch -t "$ts" {} +
}

# Write a tracked-but-uncommitted edit into a worktree.
dirty_worktree() {
    local wt="$1"
    echo "dirty" >> "$wt/README.md"
}

# Create an unpushed local commit in a worktree.
unpushed_commit_in_worktree() {
    local wt="$1"
    echo "local only" > "$wt/local.txt"
    git -C "$wt" add local.txt
    git -C "$wt" commit -qm "local only"
}
