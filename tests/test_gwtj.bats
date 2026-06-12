#!/usr/bin/env bats
# ~/.dotfiles/tests/test_gwtj.bats
# Unit tests for the pure gwtj helpers (no network / no keychain):
# _gwtj_key, _gwtj_slug, _gwtj_context_body.

load 'test_helper.bash'

setup() {
    # Pin credentials so _gwtj_context_body's URL is deterministic regardless
    # of the developer's environment.
    JIRA_BASE_URL="https://jira.example.com"
    JIRA_EMAIL="you@example.com"
    source_gwtj
}

# --- _gwtj_key: extract issue key from a URL or bare input ---

@test "_gwtj_key: extracts key from a browse URL" {
    run _gwtj_key 'https://jira.example.com/browse/PROJ-1234'
    [ "$output" = "PROJ-1234" ]
}

@test "_gwtj_key: ignores query string" {
    run _gwtj_key 'https://jira.example.com/browse/PROJ-1234?foo=bar'
    [ "$output" = "PROJ-1234" ]
}

@test "_gwtj_key: accepts a bare key" {
    run _gwtj_key 'PROJ-1234'
    [ "$output" = "PROJ-1234" ]
}

@test "_gwtj_key: uppercases a lowercase key" {
    run _gwtj_key 'proj-1234'
    [ "$output" = "PROJ-1234" ]
}

@test "_gwtj_key: handles a numeric project key" {
    run _gwtj_key 'ABC2-10'
    [ "$output" = "ABC2-10" ]
}

@test "_gwtj_key: empty when no key present" {
    run _gwtj_key 'not-a-key'
    [ "$output" = "" ]
}

# --- _gwtj_slug: slugify a summary read from stdin ---

@test "_gwtj_slug: basic slug" {
    run _gwtj_slug <<< "Fix login redirect"
    [ "$output" = "fix-login-redirect" ]
}

@test "_gwtj_slug: strips punctuation" {
    run _gwtj_slug <<< "Fix the bug!!!"
    [ "$output" = "fix-the-bug" ]
}

@test "_gwtj_slug: trims leading/trailing separators" {
    run _gwtj_slug <<< "  Hello, World.  "
    [ "$output" = "hello-world" ]
}

@test "_gwtj_slug: collapses runs of separators" {
    run _gwtj_slug <<< "a    /  b"
    [ "$output" = "a-b" ]
}

@test "_gwtj_slug: truncates long slug at a word boundary" {
    run _gwtj_slug <<< "Country code mistakenly attaching to CR tags in Zendesk"
    [ "$output" = "country-code-mistakenly-attaching-to-cr-tags-in" ]
    [ "${#output}" -le 50 ]
}

@test "_gwtj_slug: keeps a slug whose boundary lands exactly at the cap" {
    run _gwtj_slug <<< "abcd abcd abcd abcd abcd abcd abcd abcd abcd abcd extra"
    [ "$output" = "abcd-abcd-abcd-abcd-abcd-abcd-abcd-abcd-abcd-abcd" ]
}

@test "_gwtj_slug: short input is unchanged" {
    run _gwtj_slug <<< "Fix login redirect"
    [ "$output" = "fix-login-redirect" ]
}

# --- _gwtj_context_body: CLAUDE.local.md content ---

@test "_gwtj_context_body: full (key, url, status, summary)" {
    run _gwtj_context_body 'PROJ-1234' 'In Progress' 'Country code mistakenly attaching to CR tags in Zendesk'
    expected=$(printf '%s\n' \
        '# Jira: PROJ-1234' \
        'https://jira.example.com/browse/PROJ-1234' \
        'Status: In Progress' \
        'Summary: Country code mistakenly attaching to CR tags in Zendesk' \
        '' \
        'For the full description, comments, and live status, fetch this ticket via the Atlassian MCP plugin (or open the link above).')
    [ "$output" = "$expected" ]
}

@test "_gwtj_context_body: minimal (omits empty status/summary)" {
    run _gwtj_context_body 'PROJ-1' '' ''
    expected=$(printf '%s\n' \
        '# Jira: PROJ-1' \
        'https://jira.example.com/browse/PROJ-1' \
        '' \
        'For the full description, comments, and live status, fetch this ticket via the Atlassian MCP plugin (or open the link above).')
    [ "$output" = "$expected" ]
}
