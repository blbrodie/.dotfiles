#!/usr/bin/env bats
# ~/.dotfiles/tests/test_gwt_clean.bats

load 'test_helper.bash'

setup() {
    TEST_REPO=$(create_test_repo)
    source_gwt_clean
}

teardown() {
    cleanup_test_repo "$TEST_REPO"
}

@test "test_helper creates a repo with a main branch" {
    run git -C "$TEST_REPO" rev-parse --abbrev-ref HEAD
    [ "$status" -eq 0 ]
    [ "$output" = "main" ]
}

@test "_gwt_clean_default_branch returns main when main exists" {
    cd "$TEST_REPO"
    run _gwt_clean_default_branch
    [ "$status" -eq 0 ]
    [ "$output" = "main" ]
}

@test "_gwt_clean_default_branch returns master when only master exists" {
    cd "$TEST_REPO"
    git branch -m main master
    run _gwt_clean_default_branch
    [ "$status" -eq 0 ]
    [ "$output" = "master" ]
}

@test "_gwt_clean_default_branch returns empty when neither exists" {
    cd "$TEST_REPO"
    git branch -m main dev
    run _gwt_clean_default_branch
    [ "$status" -eq 0 ]
    [ "$output" = "" ]
}

@test "_gwt_clean_is_clean: pushed & untouched worktree is clean" {
    create_worktree "$TEST_REPO" feat/a
    run _gwt_clean_is_clean "$TEST_REPO/worktrees/feat/a"
    [ "$status" -eq 0 ]
}

@test "_gwt_clean_is_clean: uncommitted changes => not clean" {
    create_worktree "$TEST_REPO" feat/a
    dirty_worktree "$TEST_REPO/worktrees/feat/a"
    run _gwt_clean_is_clean "$TEST_REPO/worktrees/feat/a"
    [ "$status" -eq 1 ]
    [[ "$output" == *"uncommitted"* ]]
}

@test "_gwt_clean_is_clean: no upstream => not clean" {
    create_worktree "$TEST_REPO" feat/a --no-push
    run _gwt_clean_is_clean "$TEST_REPO/worktrees/feat/a"
    [ "$status" -eq 1 ]
    [[ "$output" == *"no upstream"* ]]
}

@test "_gwt_clean_is_clean: unpushed commits => not clean" {
    create_worktree "$TEST_REPO" feat/a
    unpushed_commit_in_worktree "$TEST_REPO/worktrees/feat/a"
    run _gwt_clean_is_clean "$TEST_REPO/worktrees/feat/a"
    [ "$status" -eq 1 ]
    [[ "$output" == *"unpushed"* ]]
}

@test "_gwt_clean_is_merged: branch merged into main => merged" {
    create_worktree "$TEST_REPO" feat/a
    merge_branch_to_main "$TEST_REPO" feat/a
    cd "$TEST_REPO"
    run _gwt_clean_is_merged feat/a main
    [ "$status" -eq 0 ]
}

@test "_gwt_clean_is_merged: unmerged branch with live remote => not merged" {
    create_worktree "$TEST_REPO" feat/a
    cd "$TEST_REPO"
    run _gwt_clean_is_merged feat/a main
    [ "$status" -eq 1 ]
}

@test "_gwt_clean_is_merged: upstream [gone] => merged" {
    create_worktree "$TEST_REPO" feat/a
    delete_remote_branch "$TEST_REPO" feat/a
    cd "$TEST_REPO"
    run _gwt_clean_is_merged feat/a main
    [ "$status" -eq 0 ]
}

@test "_gwt_clean_is_merged: empty default_branch still detects [gone]" {
    create_worktree "$TEST_REPO" feat/a
    delete_remote_branch "$TEST_REPO" feat/a
    cd "$TEST_REPO"
    run _gwt_clean_is_merged feat/a ""
    [ "$status" -eq 0 ]
}

@test "_gwt_clean_newest_mtime: reflects recent git activity" {
    create_worktree "$TEST_REPO" feat/a
    local wt="$TEST_REPO/worktrees/feat/a"
    set_path_age_days "$wt" 200
    # Simulate recent git activity: touch HEAD in the gitdir.
    local gitdir; gitdir=$(git -C "$wt" rev-parse --git-dir)
    touch "$gitdir/HEAD"
    run _gwt_clean_newest_mtime "$wt"
    [ "$status" -eq 0 ]
    local now=$(date +%s)
    [ "$((now - output))" -lt 60 ]
}

@test "_gwt_clean_is_stale: all files old => stale" {
    create_worktree "$TEST_REPO" feat/a
    local wt="$TEST_REPO/worktrees/feat/a"
    set_path_age_days "$wt" 200
    run _gwt_clean_is_stale "$wt" 120
    [ "$status" -eq 0 ]
}

@test "_gwt_clean_is_stale: recent git activity => not stale" {
    create_worktree "$TEST_REPO" feat/a
    local wt="$TEST_REPO/worktrees/feat/a"
    set_path_age_days "$wt" 200
    local gitdir; gitdir=$(git -C "$wt" rev-parse --git-dir)
    touch "$gitdir/HEAD"
    run _gwt_clean_is_stale "$wt" 120
    [ "$status" -eq 1 ]
}

@test "_gwt_clean_is_stale: respects configurable threshold" {
    create_worktree "$TEST_REPO" feat/a
    local wt="$TEST_REPO/worktrees/feat/a"
    set_path_age_days "$wt" 30
    run _gwt_clean_is_stale "$wt" 120
    [ "$status" -eq 1 ]
    run _gwt_clean_is_stale "$wt" 14
    [ "$status" -eq 0 ]
}

@test "_gwt_clean_age_days: reports integer age in days" {
    create_worktree "$TEST_REPO" feat/a
    local wt="$TEST_REPO/worktrees/feat/a"
    set_path_age_days "$wt" 45
    run _gwt_clean_age_days "$wt"
    [ "$status" -eq 0 ]
    [ "$output" -ge 44 ] && [ "$output" -le 46 ]
}

@test "gwt-clean dry-run: marks merged+clean as DELETE" {
    create_worktree "$TEST_REPO" feat/a
    merge_branch_to_main "$TEST_REPO" feat/a
    cd "$TEST_REPO"
    run gwt-clean
    [ "$status" -eq 0 ]
    [[ "$output" == *"DELETE"* ]]
    [[ "$output" == *"feat/a"* ]]
    [[ "$output" == *"merged"* ]]
    [[ "$output" == *"dry run"* ]]
}

@test "gwt-clean dry-run: marks stale+clean as DELETE" {
    create_worktree "$TEST_REPO" feat/old
    set_path_age_days "$TEST_REPO/worktrees/feat/old" 200
    cd "$TEST_REPO"
    run gwt-clean
    [ "$status" -eq 0 ]
    [[ "$output" == *"DELETE"* ]]
    [[ "$output" == *"feat/old"* ]]
    [[ "$output" == *"stale"* ]]
}

@test "gwt-clean dry-run: marks dirty as KEEP: dirty" {
    create_worktree "$TEST_REPO" feat/a
    merge_branch_to_main "$TEST_REPO" feat/a
    dirty_worktree "$TEST_REPO/worktrees/feat/a"
    cd "$TEST_REPO"
    run gwt-clean
    [ "$status" -eq 0 ]
    [[ "$output" == *"KEEP: dirty"* ]]
    [[ "$output" == *"uncommitted"* ]]
}

@test "gwt-clean dry-run: marks recent unmerged as KEEP: active" {
    create_worktree "$TEST_REPO" feat/wip
    cd "$TEST_REPO"
    run gwt-clean
    [ "$status" -eq 0 ]
    [[ "$output" == *"KEEP: active"* ]]
    [[ "$output" == *"feat/wip"* ]]
}

@test "gwt-clean dry-run: marks current worktree as KEEP: current" {
    create_worktree "$TEST_REPO" feat/a
    merge_branch_to_main "$TEST_REPO" feat/a
    cd "$TEST_REPO/worktrees/feat/a"
    run gwt-clean
    [ "$status" -eq 0 ]
    [[ "$output" == *"KEEP: current"* ]]
    [[ "$output" == *"feat/a"* ]]
}

@test "gwt-clean dry-run: respects --stale-days" {
    create_worktree "$TEST_REPO" feat/month-old
    set_path_age_days "$TEST_REPO/worktrees/feat/month-old" 30
    cd "$TEST_REPO"
    run gwt-clean --stale-days 14
    [ "$status" -eq 0 ]
    [[ "$output" == *"DELETE"* ]]
    [[ "$output" == *"feat/month-old"* ]]
}

@test "gwt-clean dry-run: does NOT delete without --force" {
    create_worktree "$TEST_REPO" feat/a
    merge_branch_to_main "$TEST_REPO" feat/a
    cd "$TEST_REPO"
    run gwt-clean
    [ "$status" -eq 0 ]
    [ -d "$TEST_REPO/worktrees/feat/a" ]
}

@test "gwt-clean dry-run: prints summary line" {
    create_worktree "$TEST_REPO" feat/a
    merge_branch_to_main "$TEST_REPO" feat/a
    cd "$TEST_REPO"
    run gwt-clean
    [[ "$output" == *"1 will be deleted"* ]]
}

@test "gwt-clean --force: removes merged+clean worktree" {
    create_worktree "$TEST_REPO" feat/a
    merge_branch_to_main "$TEST_REPO" feat/a
    cd "$TEST_REPO"
    run gwt-clean --force
    [ "$status" -eq 0 ]
    [ ! -d "$TEST_REPO/worktrees/feat/a" ]
    [[ "$output" == *"Deleted 1 worktree"* ]]
}

@test "gwt-clean --force: removes stale+clean worktree" {
    create_worktree "$TEST_REPO" feat/old
    set_path_age_days "$TEST_REPO/worktrees/feat/old" 200
    cd "$TEST_REPO"
    run gwt-clean --force
    [ "$status" -eq 0 ]
    [ ! -d "$TEST_REPO/worktrees/feat/old" ]
}

@test "gwt-clean --force: deletes local branch if merged" {
    create_worktree "$TEST_REPO" feat/a
    merge_branch_to_main "$TEST_REPO" feat/a
    cd "$TEST_REPO"
    run gwt-clean --force
    [ "$status" -eq 0 ]
    run git -C "$TEST_REPO" show-ref --verify --quiet refs/heads/feat/a
    [ "$status" -ne 0 ]  # branch is gone
}

@test "gwt-clean --force: keeps local branch for stale-but-unmerged" {
    create_worktree "$TEST_REPO" feat/old
    set_path_age_days "$TEST_REPO/worktrees/feat/old" 200
    cd "$TEST_REPO"
    run gwt-clean --force
    [ "$status" -eq 0 ]
    run git -C "$TEST_REPO" show-ref --verify --quiet refs/heads/feat/old
    [ "$status" -eq 0 ]  # branch still exists
}

@test "gwt-clean --force: skips dirty worktrees" {
    create_worktree "$TEST_REPO" feat/a
    merge_branch_to_main "$TEST_REPO" feat/a
    dirty_worktree "$TEST_REPO/worktrees/feat/a"
    cd "$TEST_REPO"
    run gwt-clean --force
    [ "$status" -eq 0 ]
    [ -d "$TEST_REPO/worktrees/feat/a" ]
}

@test "gwt-clean --force: does not delete current worktree" {
    create_worktree "$TEST_REPO" feat/a
    merge_branch_to_main "$TEST_REPO" feat/a
    cd "$TEST_REPO/worktrees/feat/a"
    run gwt-clean --force
    [ "$status" -eq 0 ]
    [ -d "$TEST_REPO/worktrees/feat/a" ]
}

@test "gwt-clean: errors outside a git repo" {
    cd /tmp
    run gwt-clean
    [ "$status" -eq 1 ]
    [[ "$output" == *"Not in a git repository"* ]]
}

@test "gwt-clean: prints 'nothing to clean' when no worktrees dir" {
    cd "$TEST_REPO"
    run gwt-clean
    [ "$status" -eq 0 ]
    [[ "$output" == *"No worktrees to clean"* ]]
}

@test "gwt-clean: rejects unknown option" {
    cd "$TEST_REPO"
    run gwt-clean --bogus
    [ "$status" -eq 2 ]
}

@test "gwt-clean: works with branch names containing '/'" {
    create_worktree "$TEST_REPO" feat/nested/deep
    merge_branch_to_main "$TEST_REPO" feat/nested/deep
    cd "$TEST_REPO"
    run gwt-clean --force
    [ "$status" -eq 0 ]
    [ ! -d "$TEST_REPO/worktrees/feat/nested/deep" ]
}

@test "gwt-clean: --help prints usage and exits 0" {
    run gwt-clean --help
    [ "$status" -eq 0 ]
    [[ "$output" == *"Usage: gwt-clean"* ]]
}

# --- --check-merged-prs ---
# These tests mock `gh` by writing a stub into a temp dir and prepending
# it to PATH. The stub returns a pre-configured list of merged PR head
# refs (one per line), records each 'gh pr list' invocation to a file so
# tests can assert batching behavior, and exits 0 for `gh auth status`.

_setup_gh_mock() {
    # Usage: _setup_gh_mock [merged_branch...]
    # Any branches passed are echoed by `gh pr list ...`, simulating the
    # `--jq '.[].headRefName'` output of a real call.
    GH_MOCK_DIR=$(mktemp -d)
    : > "$GH_MOCK_DIR/calls"
    printf '%s\n' "$@" > "$GH_MOCK_DIR/merged_branches"
    cat > "$GH_MOCK_DIR/gh" <<MOCKEOF
#!/bin/bash
if [ "\$1" = "auth" ] && [ "\$2" = "status" ]; then
    exit 0
fi
if [ "\$1" = "pr" ] && [ "\$2" = "list" ]; then
    echo "list" >> "$GH_MOCK_DIR/calls"
    cat "$GH_MOCK_DIR/merged_branches"
    exit 0
fi
exit 1
MOCKEOF
    chmod +x "$GH_MOCK_DIR/gh"
    export PATH="$GH_MOCK_DIR:$PATH"
}

_teardown_gh_mock() {
    rm -rf "$GH_MOCK_DIR"
}

@test "_gwt_clean_pr_is_merged: returns 0 when branch is in loaded list" {
    _gwt_clean_merged_prs=" feat/a feat/b "
    run _gwt_clean_pr_is_merged feat/a
    [ "$status" -eq 0 ]
}

@test "_gwt_clean_pr_is_merged: returns 1 when branch is not in loaded list" {
    _gwt_clean_merged_prs=" feat/a feat/b "
    run _gwt_clean_pr_is_merged other/branch
    [ "$status" -eq 1 ]
}

@test "_gwt_clean_pr_is_merged: does not false-match on substrings" {
    _gwt_clean_merged_prs=" feat/a "
    run _gwt_clean_pr_is_merged feat/abc
    [ "$status" -eq 1 ]
}

@test "_gwt_clean_load_merged_prs: echoes space-padded list from gh" {
    _setup_gh_mock "feat/a" "feat/b"
    run _gwt_clean_load_merged_prs
    [ "$status" -eq 0 ]
    [[ "$output" == *" feat/a "* ]]
    [[ "$output" == *" feat/b "* ]]
    _teardown_gh_mock
}

@test "gwt-clean --check-merged-prs: reclassifies no-upstream+merged as DELETE" {
    _setup_gh_mock "feat/a"
    create_worktree "$TEST_REPO" feat/a --no-push
    cd "$TEST_REPO"
    run gwt-clean --check-merged-prs
    [ "$status" -eq 0 ]
    [[ "$output" == *"DELETE"* ]]
    [[ "$output" == *"feat/a"* ]]
    [[ "$output" == *"merged PR"* ]]
    _teardown_gh_mock
}

@test "gwt-clean --check-merged-prs: leaves no-upstream+unmerged as KEEP: dirty" {
    _setup_gh_mock "feat/merged-elsewhere"
    create_worktree "$TEST_REPO" feat/a --no-push
    cd "$TEST_REPO"
    run gwt-clean --check-merged-prs
    [ "$status" -eq 0 ]
    [[ "$output" == *"KEEP: dirty"* ]]
    [[ "$output" == *"feat/a"* ]]
    [[ "$output" == *"no upstream"* ]]
    _teardown_gh_mock
}

@test "gwt-clean --check-merged-prs: calls 'gh pr list' only once (batched)" {
    _setup_gh_mock "feat/a"
    create_worktree "$TEST_REPO" feat/a --no-push
    create_worktree "$TEST_REPO" feat/b --no-push
    create_worktree "$TEST_REPO" feat/c --no-push
    cd "$TEST_REPO"
    run gwt-clean --check-merged-prs
    [ "$status" -eq 0 ]
    local count
    count=$(grep -c '^list' "$GH_MOCK_DIR/calls" 2>/dev/null)
    [ "$count" -eq 1 ]
    _teardown_gh_mock
}

@test "gwt-clean: without --check-merged-prs flag, gh is not consulted" {
    # No mock — ensure the flag is opt-in.
    create_worktree "$TEST_REPO" feat/a --no-push
    cd "$TEST_REPO"
    run gwt-clean
    [ "$status" -eq 0 ]
    [[ "$output" == *"KEEP: dirty"* ]]
    [[ "$output" == *"no upstream"* ]]
}
