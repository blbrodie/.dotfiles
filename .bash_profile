# The following lines were added by Docker Desktop to add commands to your PATH.
export PATH="$PATH:/Users/ben/.docker/bin"
# End of Docker Desktop section.

if [ -r ~/.bashrc ]; then
   source ~/.bashrc
fi

export PATH="$HOME/.poetry/bin:$PATH"

# uv
export PATH="/Users/ben/.local/bin:$PATH"

. "$HOME/.local/bin/env"
