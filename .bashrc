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

# eval $(perl -I ~/perl5/lib/perl5/ -Mlocal::lib)
function gitname {
  git config --replace-all user.name $1
}

function killport {
  lsof -i :"$1" | tail -1 | awk '{ print $2 }' | xargs kill
}

export AWS_PROFILE=whatnot_eng_user


ds() { docker ps -a | awk '{print $1}' | grep -v CONTAINER | xargs docker stop; }
drm() { docker ps -a | awk '{print $1}' | grep -v CONTAINER | xargs docker rm -f; }
drmi() { docker images | awk '{print $3}' | grep -v IMAGE | xargs docker rmi -f; }
drmv() { docker volume rm $(docker volume ls -q); }
nuke-docker() { ds && drm && drmi && drmv; }

alias mbe="cd ~/dev/whatnot_backend/"
alias live="cd ~/dev/whatnot_live/"


# aws sso login --profile whatnot_eng_user
# aws sso login
# aws --profile whatnot_eng_user sts get-caller-identity
aws_profile() {
    grep profile ~/.aws/config  | awk '{print $2}' | tr -d ']'
    printf "Please select: "
    read aws_profile

    export AWS_PROFILE=$aws_profile
}

a_pod() {
  kubectl exec -ti $(kubectl get pods | grep -v NAME | awk '{print $1}' | fzf) -- /bin/sh
}

export PATH=$PATH:$(brew --prefix)/opt/python/libexec/bin
export PATH=$PATH:~/.emacs.d/bin

# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi

# End Nix
# shellcheck source=~/.bashrc.local
source ~/.bashrc.local

eval "$(direnv hook bash)"

function curltime {
  curl -w @- -o /dev/null -s "$@" <<'EOF'
    time_namelookup:  %{time_namelookup}\n
       time_connect:  %{time_connect}\n
    time_appconnect:  %{time_appconnect}\n
   time_pretransfer:  %{time_pretransfer}\n
      time_redirect:  %{time_redirect}\n
 time_starttransfer:  %{time_starttransfer}\n
                    ----------\n
         time_total:  %{time_total}\n
EOF
}
