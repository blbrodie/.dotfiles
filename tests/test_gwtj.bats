#!/usr/bin/env bats
# ~/.dotfiles/tests/test_gwtj.bats
# Unit tests for the pure gwtj helpers (no network / no keychain):
# __gwtj_key, __gwtj_slug, __gwtj_context_body.

load 'test_helper.bash'

setup() {
    # Pin credentials so __gwtj_context_body's URL is deterministic regardless
    # of the developer's environment.
    JIRA_BASE_URL="https://jira.example.com"
    JIRA_EMAIL="you@example.com"
    source_git_worktree
}

# --- __gwtj_key: extract issue key from a URL or bare input ---

@test "__gwtj_key: extracts key from a browse URL" {
    run __gwtj_key 'https://jira.example.com/browse/PROJ-1234'
    [ "$output" = "PROJ-1234" ]
}

@test "__gwtj_key: ignores query string" {
    run __gwtj_key 'https://jira.example.com/browse/PROJ-1234?foo=bar'
    [ "$output" = "PROJ-1234" ]
}

@test "__gwtj_key: accepts a bare key" {
    run __gwtj_key 'PROJ-1234'
    [ "$output" = "PROJ-1234" ]
}

@test "__gwtj_key: uppercases a lowercase key" {
    run __gwtj_key 'proj-1234'
    [ "$output" = "PROJ-1234" ]
}

@test "__gwtj_key: handles a numeric project key" {
    run __gwtj_key 'ABC2-10'
    [ "$output" = "ABC2-10" ]
}

@test "__gwtj_key: empty when no key present" {
    run __gwtj_key 'not-a-key'
    [ "$output" = "" ]
}

# --- __gwtj_slug: slugify a summary read from stdin ---

@test "__gwtj_slug: basic slug" {
    run __gwtj_slug <<< "Fix login redirect"
    [ "$output" = "fix-login-redirect" ]
}

@test "__gwtj_slug: strips punctuation" {
    run __gwtj_slug <<< "Fix the bug!!!"
    [ "$output" = "fix-the-bug" ]
}

@test "__gwtj_slug: trims leading/trailing separators" {
    run __gwtj_slug <<< "  Hello, World.  "
    [ "$output" = "hello-world" ]
}

@test "__gwtj_slug: collapses runs of separators" {
    run __gwtj_slug <<< "a    /  b"
    [ "$output" = "a-b" ]
}

@test "__gwtj_slug: truncates long slug at a word boundary" {
    run __gwtj_slug <<< "Country code mistakenly attaching to CR tags in Zendesk"
    [ "$output" = "country-code-mistakenly-attaching-to-cr-tags-in" ]
    [ "${#output}" -le 50 ]
}

@test "__gwtj_slug: keeps a slug whose boundary lands exactly at the cap" {
    run __gwtj_slug <<< "abcd abcd abcd abcd abcd abcd abcd abcd abcd abcd extra"
    [ "$output" = "abcd-abcd-abcd-abcd-abcd-abcd-abcd-abcd-abcd-abcd" ]
}

@test "__gwtj_slug: short input is unchanged" {
    run __gwtj_slug <<< "Fix login redirect"
    [ "$output" = "fix-login-redirect" ]
}

# --- __gwtj_context_body: CLAUDE.local.md content ---

@test "__gwtj_context_body: full (key, url, status, summary)" {
    run __gwtj_context_body 'PROJ-1234' 'In Progress' 'Country code mistakenly attaching to CR tags in Zendesk'
    expected=$(printf '%s\n' \
        '# Jira: PROJ-1234' \
        'https://jira.example.com/browse/PROJ-1234' \
        'Status: In Progress' \
        'Summary: Country code mistakenly attaching to CR tags in Zendesk' \
        '' \
        'For the full description, comments, and live status, fetch this ticket via the Atlassian MCP plugin (or open the link above).')
    [ "$output" = "$expected" ]
}

@test "__gwtj_context_body: minimal (omits empty status/summary)" {
    run __gwtj_context_body 'PROJ-1' '' ''
    expected=$(printf '%s\n' \
        '# Jira: PROJ-1' \
        'https://jira.example.com/browse/PROJ-1' \
        '' \
        'For the full description, comments, and live status, fetch this ticket via the Atlassian MCP plugin (or open the link above).')
    [ "$output" = "$expected" ]
}
