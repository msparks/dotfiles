#!/bin/bash
cd $(dirname $0)

rsync -av .zsh* .vim* .gitconfig .gitignore_global .screenrc .tmux.conf ~

# Emacs configuration.
rsync -av .emacs ~
rsync -av .emacs.d ~
rsync -av --delete --exclude="default.el" .emacs.d/lisp ~/.emacs.d
rm -rf ~/.emacs-lisp ~/.emacs-mail

mkdir -m 700 -p ~/.gnupg
rsync -av .gnupg/gpg.conf ~/.gnupg

# vim stuff.
mkdir -p ~/.vim/backup ~/.vim/temp

if [ `uname -s` == "Darwin" ]; then
  rsync -ruv Library ~
fi
