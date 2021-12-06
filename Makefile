all: brew emacs bash tmux

brew:
	brew bundle

emacs:
	mkdir -p ~/.emacs.d
	ln -s $$(pwd)/init.el ~/.emacs.d/init.el

bash:
	ln -s $$(pwd)/.bash_profile ~/.bash_profile
	ln -s $$(pwd)/.bashrc ~/.bashrc

tmux:
	ln -s $$(pwd)/.tmux.conf ~/.tmux.conf
