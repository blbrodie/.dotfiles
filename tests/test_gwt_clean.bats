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

@test "_gwt_clean_newest_mtime: returns newest file mtime in tree" {
    create_worktree "$TEST_REPO" feat/a
    local wt="$TEST_REPO/worktrees/feat/a"
    set_path_age_days "$wt" 200
    # Now touch one file to "now"
    touch "$wt/README.md" 2>/dev/null || touch "$wt/.branch"
    run _gwt_clean_newest_mtime "$wt"
    [ "$status" -eq 0 ]
    # Should be within 60s of now
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

@test "_gwt_clean_is_stale: one recent file => not stale" {
    create_worktree "$TEST_REPO" feat/a
    local wt="$TEST_REPO/worktrees/feat/a"
    set_path_age_days "$wt" 200
    touch "$wt/.branch"
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

@test "_gwt_clean_dir_size_kb: reports size in KB" {
    mkdir -p "$TEST_REPO/bigdir"
    dd if=/dev/zero of="$TEST_REPO/bigdir/blob" bs=1024 count=100 >/dev/null 2>&1
    run _gwt_clean_dir_size_kb "$TEST_REPO/bigdir"
    [ "$status" -eq 0 ]
    [ "$output" -ge 100 ]
    [ "$output" -lt 200 ]
}

@test "_gwt_clean_format_kb: KB < 1024 shows K" {
    run _gwt_clean_format_kb 512
    [ "$output" = "512K" ]
}

@test "_gwt_clean_format_kb: KB >= 1024 shows M" {
    run _gwt_clean_format_kb 2048
    [ "$output" = "2M" ]
}

@test "_gwt_clean_format_kb: KB >= 1024*1024 shows G" {
    run _gwt_clean_format_kb $((3 * 1024 * 1024))
    [ "$output" = "3G" ]
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
    [[ "$output" == *"Reclaimable"* ]]
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
