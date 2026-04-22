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
