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

export JAVA_HOME=$(/usr/libexec/java_home -Fv 13 2>/dev/null)

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

function gitname {
  git config --replace-all user.name $1
}


source ~/.bashrc.local
