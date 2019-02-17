#!/bin/bash
cd $(dirname $0)

LEAD="### BEGIN GENERATED CONTENT"
TAIL="### END GENERATED CONTENT"

rsync -av .zsh* .vim* .gitconfig .gitignore_global .screenrc .tmux.conf ~

# Emacs configuration.
rsync -av .emacs ~
rsync -av .emacs.d ~
rsync -av --delete --exclude="default.el" .emacs.d/lisp ~/.emacs.d
rm -rf ~/.emacs-lisp ~/.emacs-mail

# ssh config.
mkdir -m 700 -p ~/.ssh
if [ -z "$(grep "$LEAD" ~/.ssh/config)" ]; then
  echo $LEAD >> ~/.ssh/config
  echo $TAIL >> ~/.ssh/config
fi
sed -e "/^$LEAD$/,/^$TAIL$/{ /^$LEAD$/{p; r .ssh/config.common
        }; /^$TAIL$/p; d; }" ~/.ssh/config > ~/.ssh/config.new
if [[ $? == 0 ]]; then
  mv ~/.ssh/config.new ~/.ssh/config
fi

mkdir -m 700 -p ~/.gnupg
rsync -av .gnupg/gpg.conf ~/.gnupg

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
