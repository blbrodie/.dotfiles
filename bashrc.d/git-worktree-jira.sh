# gwtj: create a git worktree from a Jira issue.
# Branch name = <KEY>-<slugified-summary>, e.g. PROJ-1234-fix-login-redirect.
# Credentials come from env; set JIRA_BASE_URL and JIRA_EMAIL in ~/.bashrc.local.
# If JIRA_API_TOKEN is unset, the token is read from the macOS keychain.
# One-time setup (create a token at id.atlassian.com -> Security -> API tokens):
#   security add-generic-password -s jira-api-token -a "$JIRA_EMAIL" -w
JIRA_BASE_URL="${JIRA_BASE_URL:-https://your-org.atlassian.net}"
JIRA_EMAIL="${JIRA_EMAIL:-you@example.com}"

_gwtj_key() {
    # Extract an ISSUE-123 key from a Jira URL or bare key, normalized to uppercase.
    printf '%s' "$1" | grep -oiE '[A-Z][A-Z0-9]+-[0-9]+' | head -n1 | tr '[:lower:]' '[:upper:]'
}

_gwtj_slug() {
    # Slugify stdin: lowercase, non-alnum -> '-', collapse/trim.
    # If longer than 50 chars, truncate at a word ('-') boundary.
    local slug
    slug=$(tr '[:upper:]' '[:lower:]' | sed -E 's/[^a-z0-9]+/-/g; s/^-+//; s/-+$//')
    if [ "${#slug}" -le 50 ]; then
        printf '%s' "$slug"
        return
    fi
    local head="${slug:0:50}"
    # Drop a trailing partial word unless char 51 already starts a new word.
    [ "${slug:50:1}" != "-" ] && head="${head%-*}"
    head="${head%-}"
    # Fallback: a single word longer than 50 chars has no boundary to cut on.
    [ -z "$head" ] && head="${slug:0:50}"
    printf '%s' "$head"
}

_gwtj_token() {
    # Prefer JIRA_API_TOKEN; fall back to the macOS keychain.
    if [ -n "${JIRA_API_TOKEN:-}" ]; then
        printf '%s' "$JIRA_API_TOKEN"
        return 0
    fi
    security find-generic-password -s jira-api-token -a "$JIRA_EMAIL" -w 2>/dev/null
}

_gwtj_context_body() {
    # Render CLAUDE.local.md body. Args: <key> <status> <summary>.
    local key="$1" status="$2" summary="$3"
    printf '# Jira: %s\n' "$key"
    printf '%s/browse/%s\n' "$JIRA_BASE_URL" "$key"
    [ -n "$status" ] && printf 'Status: %s\n' "$status"
    [ -n "$summary" ] && printf 'Summary: %s\n' "$summary"
    printf '\nFor the full description, comments, and live status, fetch this ticket via the Atlassian MCP plugin (or open the link above).\n'
}

gwtj() {
    if [ -z "$1" ]; then
        echo "Usage: gwtj <jira-url-or-key>"
        return 1
    fi

    local key
    key=$(_gwtj_key "$1")
    if [ -z "$key" ]; then
        echo "Error: could not find a Jira issue key in '$1'" >&2
        return 1
    fi

    local token
    token=$(_gwtj_token)
    if [ -z "$token" ]; then
        echo "Error: no Jira API token found." >&2
        echo "Set JIRA_API_TOKEN, or store one in the keychain:" >&2
        echo "  security add-generic-password -s jira-api-token -a \"$JIRA_EMAIL\" -w" >&2
        return 1
    fi

    local resp summary status branch="$key"
    resp=$(curl -sf -u "$JIRA_EMAIL:$token" -H "Accept: application/json" \
        "$JIRA_BASE_URL/rest/api/3/issue/$key?fields=summary,status")
    summary=$(printf '%s' "$resp" | jq -r '(.fields.summary // "") | gsub("^\\s+|\\s+$";"")' 2>/dev/null)
    status=$(printf '%s' "$resp" | jq -r '(.fields.status.name // "") | gsub("^\\s+|\\s+$";"")' 2>/dev/null)

    if [ -n "$summary" ]; then
        local slug
        slug=$(printf '%s' "$summary" | _gwtj_slug)
        [ -n "$slug" ] && branch="$key-$slug"
    else
        echo "Warning: couldn't fetch summary for $key; using key only" >&2
    fi

    echo "Creating worktree: $branch"
    gwt "$branch" || return 1

    # gwt leaves us in the worktree root on success. Drop a Jira context file
    # that Claude Code auto-loads, and exclude it locally so it's never committed.
    if [ ! -e CLAUDE.local.md ]; then
        _gwtj_context_body "$key" "$status" "$summary" > CLAUDE.local.md
        local exclude current=""
        exclude="$(git rev-parse --git-common-dir 2>/dev/null)/info/exclude"
        [ -f "$exclude" ] && current=$(<"$exclude")
        case $'\n'"$current"$'\n' in
            *$'\n'CLAUDE.local.md$'\n'*) ;;  # already excluded
            *) [ -d "$(dirname "$exclude")" ] && printf '%s\n' 'CLAUDE.local.md' >> "$exclude" ;;
        esac
        echo "Wrote CLAUDE.local.md with $key context"
    fi
}
