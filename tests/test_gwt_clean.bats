#!/usr/bin/env bats
# ~/.dotfiles/tests/test_gwt_clean.bats

load 'test_helper.bash'

setup() {
    TEST_REPO=$(create_test_repo)
    source_git_worktree
}

teardown() {
    cleanup_test_repo "$TEST_REPO"
}

@test "test_helper creates a repo with a main branch" {
    run git -C "$TEST_REPO" rev-parse --abbrev-ref HEAD
    [ "$status" -eq 0 ]
    [ "$output" = "main" ]
}

@test "__gwt_clean_default_branch returns main when main exists" {
    cd "$TEST_REPO"
    run __gwt_clean_default_branch
    [ "$status" -eq 0 ]
    [ "$output" = "main" ]
}

@test "__gwt_clean_default_branch returns master when only master exists" {
    cd "$TEST_REPO"
    git branch -m main master
    run __gwt_clean_default_branch
    [ "$status" -eq 0 ]
    [ "$output" = "master" ]
}

@test "__gwt_clean_default_branch returns empty when neither exists" {
    cd "$TEST_REPO"
    git branch -m main dev
    run __gwt_clean_default_branch
    [ "$status" -eq 0 ]
    [ "$output" = "" ]
}

@test "__gwt_clean_is_clean: pushed & untouched worktree is clean" {
    create_worktree "$TEST_REPO" feat/a
    run __gwt_clean_is_clean "$TEST_REPO/worktrees/feat/a"
    [ "$status" -eq 0 ]
}

@test "__gwt_clean_is_clean: uncommitted changes => not clean" {
    create_worktree "$TEST_REPO" feat/a
    dirty_worktree "$TEST_REPO/worktrees/feat/a"
    run __gwt_clean_is_clean "$TEST_REPO/worktrees/feat/a"
    [ "$status" -eq 1 ]
    [[ "$output" == *"uncommitted"* ]]
}

@test "__gwt_clean_is_clean: no upstream => not clean" {
    create_worktree "$TEST_REPO" feat/a --no-push
    run __gwt_clean_is_clean "$TEST_REPO/worktrees/feat/a"
    [ "$status" -eq 1 ]
    [[ "$output" == *"no upstream"* ]]
}

@test "__gwt_clean_is_clean: unpushed commits => not clean" {
    create_worktree "$TEST_REPO" feat/a
    unpushed_commit_in_worktree "$TEST_REPO/worktrees/feat/a"
    run __gwt_clean_is_clean "$TEST_REPO/worktrees/feat/a"
    [ "$status" -eq 1 ]
    [[ "$output" == *"unpushed"* ]]
}

@test "__gwt_clean_is_merged: branch merged into main => merged" {
    create_worktree "$TEST_REPO" feat/a
    merge_branch_to_main "$TEST_REPO" feat/a
    cd "$TEST_REPO"
    run __gwt_clean_is_merged feat/a main
    [ "$status" -eq 0 ]
}

@test "__gwt_clean_is_merged: unmerged branch with live remote => not merged" {
    create_worktree "$TEST_REPO" feat/a
    cd "$TEST_REPO"
    run __gwt_clean_is_merged feat/a main
    [ "$status" -eq 1 ]
}

@test "__gwt_clean_is_merged: upstream [gone] => merged" {
    create_worktree "$TEST_REPO" feat/a
    delete_remote_branch "$TEST_REPO" feat/a
    cd "$TEST_REPO"
    run __gwt_clean_is_merged feat/a main
    [ "$status" -eq 0 ]
}

@test "__gwt_clean_is_merged: empty default_branch still detects [gone]" {
    create_worktree "$TEST_REPO" feat/a
    delete_remote_branch "$TEST_REPO" feat/a
    cd "$TEST_REPO"
    run __gwt_clean_is_merged feat/a ""
    [ "$status" -eq 0 ]
}

@test "__gwt_clean_newest_mtime: reflects recent git activity" {
    create_worktree "$TEST_REPO" feat/a
    local wt="$TEST_REPO/worktrees/feat/a"
    set_path_age_days "$wt" 200
    # Simulate recent git activity: touch HEAD in the gitdir.
    local gitdir; gitdir=$(git -C "$wt" rev-parse --git-dir)
    touch "$gitdir/HEAD"
    run __gwt_clean_newest_mtime "$wt"
    [ "$status" -eq 0 ]
    local now=$(date +%s)
    [ "$((now - output))" -lt 60 ]
}

@test "__gwt_clean_is_stale: all files old => stale" {
    create_worktree "$TEST_REPO" feat/a
    local wt="$TEST_REPO/worktrees/feat/a"
    set_path_age_days "$wt" 200
    run __gwt_clean_is_stale "$wt" 120
    [ "$status" -eq 0 ]
}

@test "__gwt_clean_is_stale: recent git activity => not stale" {
    create_worktree "$TEST_REPO" feat/a
    local wt="$TEST_REPO/worktrees/feat/a"
    set_path_age_days "$wt" 200
    local gitdir; gitdir=$(git -C "$wt" rev-parse --git-dir)
    touch "$gitdir/HEAD"
    run __gwt_clean_is_stale "$wt" 120
    [ "$status" -eq 1 ]
}

@test "__gwt_clean_is_stale: respects configurable threshold" {
    create_worktree "$TEST_REPO" feat/a
    local wt="$TEST_REPO/worktrees/feat/a"
    set_path_age_days "$wt" 30
    run __gwt_clean_is_stale "$wt" 120
    [ "$status" -eq 1 ]
    run __gwt_clean_is_stale "$wt" 14
    [ "$status" -eq 0 ]
}

@test "__gwt_clean_age_days: reports integer age in days" {
    create_worktree "$TEST_REPO" feat/a
    local wt="$TEST_REPO/worktrees/feat/a"
    set_path_age_days "$wt" 45
    run __gwt_clean_age_days "$wt"
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

# --- --check-merged-prs / --include-closed ---
# These tests mock `gh` by writing a stub into a temp dir and prepending
# it to PATH. The stub answers per-branch `gh pr list --head <b>` queries
# from a configured branch->state map, records each invocation (with the
# queried branch) so tests can assert what was consulted, and exits 0 for
# `gh auth status`.

_setup_gh_mock() {
    # Usage: _setup_gh_mock [branch:STATE ...]
    #   e.g. _setup_gh_mock feat/a:MERGED feat/b:CLOSED
    # A `gh pr list --head <b>` for a mapped branch echoes its STATE
    # (MERGED/OPEN/CLOSED); unmapped branches echo nothing (=> NONE). The
    # stub ignores --json/--jq, so gwt-clean captures the echoed state
    # directly — same observable result as the real `--jq` pipeline.
    GH_MOCK_DIR=$(mktemp -d)
    : > "$GH_MOCK_DIR/calls"
    printf '%s\n' "$@" > "$GH_MOCK_DIR/pr_map"
    cat > "$GH_MOCK_DIR/gh" <<'MOCKEOF'
#!/bin/bash
DIR="$(dirname "$0")"
if [ "$1" = "auth" ] && [ "$2" = "status" ]; then
    exit 0
fi
if [ "$1" = "pr" ] && [ "$2" = "list" ]; then
    head=""
    while [ $# -gt 0 ]; do
        case "$1" in
            --head) head="$2"; shift 2 ;;
            *) shift ;;
        esac
    done
    echo "list $head" >> "$DIR/calls"
    grep "^${head}:" "$DIR/pr_map" 2>/dev/null | head -1 | cut -d: -f2
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

@test "__gwt_clean_pr_state: reports MERGED for a merged branch" {
    _setup_gh_mock "feat/a:MERGED"
    run __gwt_clean_pr_state feat/a
    [ "$status" -eq 0 ]
    [ "$output" = "MERGED" ]
    _teardown_gh_mock
}

@test "__gwt_clean_pr_state: reports CLOSED for a closed-unmerged branch" {
    _setup_gh_mock "feat/a:CLOSED"
    run __gwt_clean_pr_state feat/a
    [ "$output" = "CLOSED" ]
    _teardown_gh_mock
}

@test "__gwt_clean_pr_state: reports NONE when no PR exists for the branch" {
    _setup_gh_mock "feat/other:MERGED"
    run __gwt_clean_pr_state feat/a
    [ "$output" = "NONE" ]
    _teardown_gh_mock
}

@test "__gwt_clean_pr_state: queries by the exact branch head" {
    _setup_gh_mock "feat/a:MERGED"
    __gwt_clean_pr_state feat/a >/dev/null
    grep -qx "list feat/a" "$GH_MOCK_DIR/calls"
    _teardown_gh_mock
}

@test "gwt-clean --check-merged-prs: merged + no-upstream + STALE => DELETE" {
    _setup_gh_mock "feat/a:MERGED"
    create_worktree "$TEST_REPO" feat/a --no-push
    set_commit_age_days "$TEST_REPO/worktrees/feat/a" 200
    cd "$TEST_REPO"
    run gwt-clean --check-merged-prs
    [ "$status" -eq 0 ]
    [[ "$output" == *"DELETE"* ]]
    [[ "$output" == *"feat/a"* ]]
    [[ "$output" == *"merged PR"* ]]
    _teardown_gh_mock
}

@test "gwt-clean --check-merged-prs: merged + no-upstream + RECENT => KEEP (protects follow-up work)" {
    # A merged PR proves the reviewed baseline is in main, but a RECENT
    # no-upstream worktree may carry post-merge commits that squash-merge
    # makes indistinguishable from merged content. A recent commit date is
    # the guard. create_worktree leaves a just-made (recent) commit.
    _setup_gh_mock "feat/a:MERGED"
    create_worktree "$TEST_REPO" feat/a --no-push
    cd "$TEST_REPO"
    run gwt-clean --check-merged-prs
    [ "$status" -eq 0 ]
    [[ "$output" == *"KEEP: dirty"* ]]
    [[ "$output" == *"feat/a"* ]]
    # Recent worktrees short-circuit before any gh query.
    [ ! -s "$GH_MOCK_DIR/calls" ]
    _teardown_gh_mock
}

@test "gwt-clean --check-merged-prs: merged + unpushed commits + stale => DELETE" {
    _setup_gh_mock "feat/a:MERGED"
    create_worktree "$TEST_REPO" feat/a
    unpushed_commit_in_worktree "$TEST_REPO/worktrees/feat/a"
    set_commit_age_days "$TEST_REPO/worktrees/feat/a" 200
    cd "$TEST_REPO"
    run gwt-clean --check-merged-prs
    [ "$status" -eq 0 ]
    [[ "$output" == *"DELETE"* ]]
    [[ "$output" == *"feat/a"* ]]
    _teardown_gh_mock
}

@test "gwt-clean --check-merged-prs: uncommitted changes are NEVER overridden, even if merged+stale" {
    _setup_gh_mock "feat/a:MERGED"
    create_worktree "$TEST_REPO" feat/a
    dirty_worktree "$TEST_REPO/worktrees/feat/a"
    set_commit_age_days "$TEST_REPO/worktrees/feat/a" 200
    cd "$TEST_REPO"
    run gwt-clean --check-merged-prs
    [ "$status" -eq 0 ]
    [[ "$output" == *"KEEP: dirty"* ]]
    [[ "$output" == *"uncommitted changes"* ]]
    _teardown_gh_mock
}

@test "gwt-clean --check-merged-prs: no PR (NONE) + stale => KEEP: dirty" {
    _setup_gh_mock "feat/other:MERGED"
    create_worktree "$TEST_REPO" feat/a --no-push
    set_commit_age_days "$TEST_REPO/worktrees/feat/a" 200
    cd "$TEST_REPO"
    run gwt-clean --check-merged-prs
    [ "$status" -eq 0 ]
    [[ "$output" == *"KEEP: dirty"* ]]
    [[ "$output" == *"feat/a"* ]]
    _teardown_gh_mock
}

@test "gwt-clean --include-closed: closed-unmerged + stale => DELETE" {
    _setup_gh_mock "feat/a:CLOSED"
    create_worktree "$TEST_REPO" feat/a --no-push
    set_commit_age_days "$TEST_REPO/worktrees/feat/a" 200
    cd "$TEST_REPO"
    run gwt-clean --include-closed
    [ "$status" -eq 0 ]
    [[ "$output" == *"DELETE"* ]]
    [[ "$output" == *"closed PR"* ]]
    _teardown_gh_mock
}

@test "gwt-clean --check-merged-prs: closed-unmerged is KEEP without --include-closed" {
    _setup_gh_mock "feat/a:CLOSED"
    create_worktree "$TEST_REPO" feat/a --no-push
    set_commit_age_days "$TEST_REPO/worktrees/feat/a" 200
    cd "$TEST_REPO"
    run gwt-clean --check-merged-prs
    [ "$status" -eq 0 ]
    [[ "$output" == *"KEEP: dirty"* ]]
    _teardown_gh_mock
}

@test "gwt-clean --force --check-merged-prs: force-deletes stale merged branch (squash case)" {
    # Squash-merged PR shape: local commits differ from main and `git
    # branch -d` would refuse; `-D` is correct because gh confirmed merge.
    _setup_gh_mock "feat/a:MERGED"
    create_worktree "$TEST_REPO" feat/a --no-push
    set_commit_age_days "$TEST_REPO/worktrees/feat/a" 200
    cd "$TEST_REPO"
    run gwt-clean --force --check-merged-prs
    [ "$status" -eq 0 ]
    [ ! -d "$TEST_REPO/worktrees/feat/a" ]
    run git -C "$TEST_REPO" show-ref --verify --quiet refs/heads/feat/a
    [ "$status" -ne 0 ]  # branch is gone
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

# --- gwt-rm: explicit removal of never-PR'd worktrees ---

@test "gwt-rm: force-removes a dirty worktree and deletes its branch" {
    create_worktree "$TEST_REPO" feat/a --no-push
    dirty_worktree "$TEST_REPO/worktrees/feat/a"
    cd "$TEST_REPO"
    run gwt-rm feat/a
    [ "$status" -eq 0 ]
    [ ! -d "$TEST_REPO/worktrees/feat/a" ]
    run git -C "$TEST_REPO" show-ref --verify --quiet refs/heads/feat/a
    [ "$status" -ne 0 ]
}

@test "gwt-rm: removes multiple worktrees in one call" {
    create_worktree "$TEST_REPO" feat/a --no-push
    create_worktree "$TEST_REPO" feat/b --no-push
    cd "$TEST_REPO"
    run gwt-rm feat/a feat/b
    [ "$status" -eq 0 ]
    [ ! -d "$TEST_REPO/worktrees/feat/a" ]
    [ ! -d "$TEST_REPO/worktrees/feat/b" ]
}

@test "gwt-rm: skips a name with no matching worktree, keeps going" {
    create_worktree "$TEST_REPO" feat/a --no-push
    cd "$TEST_REPO"
    run gwt-rm nope feat/a
    [ "$status" -eq 0 ]
    [[ "$output" == *"skip"* ]]
    [ ! -d "$TEST_REPO/worktrees/feat/a" ]
}

@test "gwt-rm: with no arguments prints usage and exits non-zero" {
    cd "$TEST_REPO"
    run gwt-rm
    [ "$status" -ne 0 ]
    [[ "$output" == *"Usage: gwt-rm"* ]]
}
