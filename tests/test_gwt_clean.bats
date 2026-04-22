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
