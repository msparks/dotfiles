#!/bin/sh
rsync -ruv .zsh* .vim* .gitconfig .screenrc ~
mkdir -p ~/.vim/backup ~/.vim/temp
