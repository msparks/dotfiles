#!/bin/bash
rsync -av .zsh* .vim* .gitconfig .gitignore_global .screenrc .tmux.conf ~
rsync -av .emacs* ~
rsync -av .ssh/config ~/.ssh

# vim stuff.
mkdir -p ~/.vim/backup ~/.vim/temp

# ssh stuff.
mkdir -m 700 -p ~/.ssh/sockets/
chmod 700 ~/.ssh/config
touch ~/.ssh/known_hosts

if [ `uname -s` == "Darwin" ]; then
  rsync -ruv Library ~
fi
