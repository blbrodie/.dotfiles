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

alias tags='git ls-files | ctags -e -R --links=no -L-'
