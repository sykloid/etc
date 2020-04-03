# Makefile: Deploys links in all the right places.
# P.C. Shyamshankar <sykora@lucentbeing.com>

all: ghc git irssi latexmk tmux vim x xmonad zsh

ghc:
	rm -rf ~/.ghci
	ln -s `pwd`/ghc/ghci ~/.ghci

git:
	rm -rf ~/.gitconfig
	ln -s `pwd`/git/gitconfig ~/.gitconfig

irssi:
	rm -rf ~/.irssi
	ln -s `pwd`/irssi ~/.irssi
	cp ~/.irssi/config.safe ~/.irssi/config

latexmk:
	rm -rf ~/.latexmkrc
	ln -s `pwd`/latexmk/latexmkrc ~/.latexmkrc

tmux:
	rm -rf ~/.tmux.conf
	ln -s `pwd`/tmux/tmux.conf ~/.tmux.conf

vim:
	rm -rf ~/.vim ~/.vimrc
	ln -s `pwd`/vim ~/.vim
	ln -s `pwd`/vim/vimrc ~/.vimrc

x:
	rm -rf ~/.xinitrc
	ln -s `pwd`/x/xinitrc ~/.xinitrc

xmonad:
	rm -rf ~/.xmonad
	ln -s `pwd`/xmonad ~/.xmonad

zsh:
	rm ~/.zshenv ~/.zlogin ~/.zprofile ~/.zshrc ~/.zlogout
	ln -s `pwd`/zsh/zprofile ~/.zprofile
