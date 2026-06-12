# Miscellaneous interactive shell helpers.

function gitname {
  git config --replace-all user.name $1
}

function killport {
  lsof -i :"$1" | tail -1 | awk '{ print $2 }' | xargs kill
}

nuke-docker() {
  docker stop $(docker ps -aq) 2>/dev/null
  docker rm $(docker ps -aq) 2>/dev/null
  docker volume rm $(docker volume ls -q) 2>/dev/null
  docker system prune -a -f
}

# aws sso login --profile <your-profile>
# aws sso login
# aws --profile <your-profile> sts get-caller-identity
aws_profile() {
    grep profile ~/.aws/config  | awk '{print $2}' | tr -d ']'
    printf "Please select: "
    read aws_profile

    export AWS_PROFILE=$aws_profile
}

a_pod() {
  kubectl exec -ti $(kubectl get pods | grep -v NAME | awk '{print $1}' | fzf) -- /bin/sh
}

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

vterm_printf() {
    if [ -n "$TMUX" ] \
        && { [ "${TERM%%-*}" = "tmux" ] \
            || [ "${TERM%%-*}" = "screen" ]; }; then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
