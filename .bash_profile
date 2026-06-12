if [ -r ~/.bashrc ]; then
   source ~/.bashrc
fi

export PATH="$HOME/.poetry/bin:$PATH"

# uv
export PATH="/Users/ben/.local/bin:$PATH"

. "$HOME/.local/bin/env"
