export BASH_SILENCE_DEPRECATION_WARNING=1
export PS1="\W\$ "
export CLICOLOR=1

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

# . /usr/local/opt/asdf/asdf.sh
# . /usr/local/opt/asdf/etc/bash_completion.d/asdf.bash

# Don't set if already set by emacs
export EDITOR=${EDITOR:='emacsclient -t --alternate-editor=""'}

# build erlang with docs
export KERL_BUILD_DOCS=yes

alias tags='git ls-files | ctags --extra=+q -e -R --links=no -L-'

eval "$(/opt/homebrew/bin/brew shellenv)"

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

# eval $(perl -I ~/perl5/lib/perl5/ -Mlocal::lib)
function gitname {
  git config --replace-all user.name $1
}

function killport {
  lsof -i :"$1" | tail -1 | awk '{ print $2 }' | xargs kill
}

# function aws_profile { export AWS_PROFILE="$1"; }
export AWS_PROFILE=whatnot_eng_admin

eval "$(direnv hook bash)"

ds() { docker ps -a | awk '{print $1}' | grep -v CONTAINER | xargs docker stop; }
drm() { docker ps -a | awk '{print $1}' | grep -v CONTAINER | xargs docker rm -f; }
drmi() { docker images | awk '{print $3}' | grep -v IMAGE | xargs docker rmi -f; }
drmv() { docker volume rm $(docker volume ls -q); }

# shellcheck source=~/.bashrc.local
source ~/.bashrc.local
