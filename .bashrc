export BASH_SILENCE_DEPRECATION_WARNING=1
export PS1="\W\$ "
export CLICOLOR=1

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

# . /usr/local/opt/asdf/asdf.sh
# . /usr/local/opt/asdf/etc/bash_completion.d/asdf.bash

# Don't set if already set by emacs
export EDITOR='emacs -nw'

# build erlang with docs
export KERL_BUILD_DOCS=yes

alias tags='git ls-files | ctags --extra=+q -e -R --links=no -L-'

eval "$(/opt/homebrew/bin/brew shellenv)"

# eval $(perl -I ~/perl5/lib/perl5/ -Mlocal::lib)

# Machine/company-specific values (AWS_PROFILE, JIRA_EMAIL, project nav
# aliases, ...) live in the untracked ~/.bashrc.local, sourced below.

export PATH=$PATH:$(brew --prefix)/opt/python/libexec/bin
export PATH=$PATH:~/.emacs.d/bin

# # Nix
# if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
#   . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
# fi
# End Nix

# shellcheck source=~/.bashrc.local
source ~/.bashrc.local

eval "$(direnv hook bash)"

export MYPY="mypy --skip-cache-mtime-checks --exclude worktrees"

# uv
export PATH="/Users/ben/.local/bin:$PATH"

. "$HOME/.local/bin/env"

alias claude='claude --model opus'

# --- load modular shell functions from bashrc.d/ ---
# Each file in bashrc.d/ is a self-contained group of functions/aliases.
# Add a new file there to add new commands — no edits to this file needed.
# Resolve the real dotfiles dir even though ~/.bashrc is a symlink.
__bashrc_src="${BASH_SOURCE[0]}"
while [ -L "$__bashrc_src" ]; do
    __bashrc_link="$(readlink "$__bashrc_src")"
    case "$__bashrc_link" in
        /*) __bashrc_src="$__bashrc_link" ;;
        *)  __bashrc_src="$(dirname "$__bashrc_src")/$__bashrc_link" ;;
    esac
done
DOTFILES_DIR="$(cd "$(dirname "$__bashrc_src")" && pwd)"
if [ -d "$DOTFILES_DIR/bashrc.d" ]; then
    for __f in "$DOTFILES_DIR"/bashrc.d/*.sh; do
        [ -r "$__f" ] && . "$__f"
    done
fi
unset __bashrc_src __bashrc_link __f
