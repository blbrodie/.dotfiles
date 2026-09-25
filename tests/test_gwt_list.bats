#!/usr/bin/env bats
# ~/.dotfiles/tests/test_gwt_list.bats
#
# __gwt_list backs both the "gwt" usage listing and the tab completion for
# gwt / gwt-rm. It must work from the repo root, not just from inside a
# worktree: `git rev-parse --git-common-dir` returns a *relative* ".git"
# at the repo root, so the worktrees prefix has to be resolved to an
# absolute path before matching it against `git worktree list` output.

load 'test_helper.bash'

setup() {
    TEST_REPO=$(create_test_repo)
    source_git_worktree
}

teardown() {
    cleanup_test_repo "$TEST_REPO"
}

@test "__gwt_list lists worktree names from the repo root" {
    create_worktree "$TEST_REPO" test-one
    create_worktree "$TEST_REPO" test-two
    cd "$TEST_REPO"
    run __gwt_list
    [ "$status" -eq 0 ]
    [[ "$output" == *"test-one"* ]]
    [[ "$output" == *"test-two"* ]]
}

@test "__gwt_list lists worktree names from inside a worktree" {
    create_worktree "$TEST_REPO" test-one
    cd "$TEST_REPO/worktrees/test-one"
    run __gwt_list
    [ "$status" -eq 0 ]
    [[ "$output" == *"test-one"* ]]
}

@test "__gwt_list keeps the full name for branches containing a slash" {
    create_worktree "$TEST_REPO" feature/foo
    cd "$TEST_REPO"
    run __gwt_list
    [ "$status" -eq 0 ]
    [[ "$output" == *"feature/foo"* ]]
}

@test "__gwt_list is empty (and succeeds) outside a git repo" {
    cd "$TMPDIR"
    run __gwt_list
    [ "$status" -eq 0 ]
    [ "$output" = "" ]
}
