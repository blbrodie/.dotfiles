#!/usr/bin/env bats
# ~/.dotfiles/tests/test_snapshot_compat.bats
#
# Claude Code does not source ~/.bashrc for its Bash tool. It snapshots the
# interactive shell once and replays that snapshot before every command. The
# snapshot generator deliberately drops single-underscore functions (to skip
# completion functions) while keeping double-underscore ones:
#
#     declare -F | cut -d' ' -f3 | grep -vE '^_[^_]' | while read func; do
#
# So a helper named _gwtj_key vanishes inside Claude Code while its public
# caller gwtj survives, and gwtj dies on "_gwtj_key: command not found".
# Every helper reachable from a public entry point must therefore be named
# __gwt*, not _gwt*. A true completion function is exempt: it is only ever
# invoked by readline in an interactive shell, which is never the snapshot.

load 'test_helper.bash'

MODULE='git-worktree.sh'

# The snapshot generator's function filter, verbatim.
claude_snapshot_filter() {
    grep -vE '^_[^_]'
}

# Function names defined by the module, in a shell with nothing else loaded.
module_functions() {
    bash --noprofile --norc -c \
        "source '${BATS_TEST_DIRNAME}/../bashrc.d/${MODULE}' >/dev/null 2>&1
         declare -F | cut -d' ' -f3"
}

# Names of gwt helpers referenced inside a function's body.
referenced_helpers() {
    bash --noprofile --norc -c \
        "source '${BATS_TEST_DIRNAME}/../bashrc.d/${MODULE}' >/dev/null 2>&1
         declare -f '$1' 2>/dev/null" \
        | grep -oE '\b_{1,2}gwt[a-zA-Z0-9_]*' | sort -u
}

@test "every helper reachable from a public entry point survives the snapshot filter" {
    # Walk the call graph out from the public commands, so a helper added
    # later is covered without editing this test.
    local -a queue=(gwt gwtj gwt-clean gwt-rm)
    local seen=" " reachable=" " fn ref
    while [ "${#queue[@]}" -gt 0 ]; do
        fn="${queue[0]}"
        queue=("${queue[@]:1}")
        case "$seen" in *" $fn "*) continue ;; esac
        seen="$seen$fn "
        while read -r ref; do
            [ -z "$ref" ] && continue
            reachable="$reachable$ref "
            queue+=("$ref")
        done < <(referenced_helpers "$fn")
    done

    # Sanity: the walk actually found helpers, so a silent empty set
    # can never make this test vacuously pass.
    [ "$(printf '%s' "$reachable" | wc -w)" -gt 5 ]

    local dropped=""
    for ref in $reachable; do
        if ! printf '%s\n' "$ref" | claude_snapshot_filter >/dev/null; then
            dropped="$dropped $ref"
        fi
    done
    [ -z "$dropped" ] || {
        echo "helpers stripped from the Claude Code snapshot:$dropped"
        echo "rename them to a __ prefix so the filter keeps them"
        false
    }
}

@test "gwtj still resolves its helpers after a snapshot round-trip" {
    # Reproduce the real failure: emit a snapshot the way Claude Code does,
    # source it into a shell that never saw ~/.bashrc, and call gwtj.
    local snapshot="$BATS_TEST_TMPDIR/snapshot.sh"
    bash --noprofile --norc -c \
        "source '${BATS_TEST_DIRNAME}/../bashrc.d/${MODULE}' >/dev/null 2>&1
         declare -F | cut -d' ' -f3 | grep -vE '^_[^_]' | while read func; do
             printf 'eval %q > /dev/null 2>&1\n' \"\$(declare -f \"\$func\")\"
         done" > "$snapshot"

    # No token available => gwtj must stop at the token check, which proves
    # key extraction ran. JIRA_EMAIL points at an account with no keychain
    # entry so this never depends on the developer's real credentials.
    run bash --noprofile --norc -c \
        "source '$snapshot'
         JIRA_API_TOKEN='' JIRA_EMAIL='no-such-account@invalid.test' gwtj PROJ-1234"

    [ "$status" -eq 1 ]
    [[ "$output" != *"command not found"* ]]
    [[ "$output" != *"could not find a Jira issue key"* ]]
    [[ "$output" == *"no Jira API token found"* ]]
}

@test "gwt lists worktrees after a snapshot round-trip" {
    local snapshot="$BATS_TEST_TMPDIR/snapshot.sh"
    bash --noprofile --norc -c \
        "source '${BATS_TEST_DIRNAME}/../bashrc.d/${MODULE}' >/dev/null 2>&1
         declare -F | cut -d' ' -f3 | grep -vE '^_[^_]' | while read func; do
             printf 'eval %q > /dev/null 2>&1\n' \"\$(declare -f \"\$func\")\"
         done" > "$snapshot"

    # Bare `gwt` prints the worktree listing, which goes through the list
    # helper -- the same helper tab completion uses.
    run bash --noprofile --norc -c "source '$snapshot'; cd '$BATS_TEST_TMPDIR'; gwt"

    [ "$status" -eq 1 ]
    [[ "$output" != *"command not found"* ]]
    [[ "$output" == *"Usage: gwt <branch_name>"* ]]
}
