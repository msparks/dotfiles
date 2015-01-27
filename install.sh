#!/bin/bash
rsync -av .zsh* .vim* .gitconfig .gitignore_global .screenrc .tmux.conf ~

# Emacs configuration.
rsync -av .emacs ~
rsync -av .emacs.d ~
rm -rf ~/.emacs-lisp ~/.emacs-mail

mkdir -m 700 -p ~/.ssh
rsync -av .ssh/config ~/.ssh

# vim stuff.
mkdir -p ~/.vim/backup ~/.vim/temp

# ssh stuff.
mkdir -m 700 -p ~/.ssh/sockets/
chmod 700 ~/.ssh/config
touch ~/.ssh/known_hosts

# oh-my-zsh plugins.
rsync -av omz-plugins ~/.zsh

if [ `uname -s` == "Darwin" ]; then
  rsync -ruv Library ~
fi
