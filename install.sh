#!/bin/bash
rsync -ruv .zsh* .vim* .gitconfig .screenrc .emacs* .ssh ~

# vim stuff.
mkdir -p ~/.vim/backup ~/.vim/temp

# ssh stuff.
mkdir -m 700 -p ~/.ssh/sockets/
touch ~/.ssh/known_hosts

if [ `uname -s` == "Darwin" ]; then
  rsync -ruv Library ~
fi
