export BASH_SILENCE_DEPRECATION_WARNING=1
export PS1="\W\$ "
export CLICOLOR=1

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

. /usr/local/opt/asdf/asdf.sh
. /usr/local/opt/asdf/etc/bash_completion.d/asdf.bash

# Don't set if already set by emacs
export EDITOR=${EDITOR:='emacsclient -t --alternate-editor=""'}

# build erlang with docs
export KERL_BUILD_DOCS=yes

alias tags='git ls-files | ctags --extra=+q -e -R --links=no -L-'

if type brew &>/dev/null; then
  HOMEBREW_PREFIX="$(brew --prefix)"
  if [[ -r "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh" ]]; then
    source "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh"
  else
    for COMPLETION in "${HOMEBREW_PREFIX}/etc/bash_completion.d/"*; do
      [[ -r "$COMPLETION" ]] && source "$COMPLETION"
    done
  fi
fi

eval $(perl -I ~/perl5/lib/perl5/ -Mlocal::lib)

function gitname {
  git config --replace-all user.name $1
}

function killport {
  lsof -i :"$1" | tail -1 | awk '{ print $2 }' | xargs kill
}


# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix

eval "$(direnv hook bash)"

# shellcheck source=~/.bashrc.local
source ~/.bashrc.local
