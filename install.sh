#!/bin/bash
rsync -ruv .zsh* .vim* .gitconfig .screenrc ~
mkdir -p ~/.vim/backup ~/.vim/temp
mkdir -p ~/.ssh
touch ~/.ssh/known_hosts

if [ `uname -s` == "Darwin" ]; then
  rsync -ruv Library ~
fi
