all: emacs bash tmux ctags

emacs:
	mkdir -p ~/.emacs.d
	ln -s $$(pwd)/init.el ~/.emacs.d/init.el

bash:
	ln -s $$(pwd)/.bash_profile ~/.bash_profile
	ln -s $$(pwd)/.bashrc ~/.bashrc

tmux:
	ln -s $$(pwd)/.tmux.conf ~/.tmux.conf

ctags:
	ln -s $$(pwd)/.ctags ~/.ctags

install: install-emacs

install-emacs:
	brew cask install emacs
