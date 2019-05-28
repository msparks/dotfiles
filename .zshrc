# ~/.zshrc
# Matt Sparks
# quadpoint.org
#
# Considerable zsh-fu from compnerd, jdong, Mako, and majnematic.

# History variables
HISTSIZE=5000
HISTFILE=~/.zsh/.history     # history file name
SAVEHIST=5000                # lines of history

# Set options
setopt   CORRECT             # correct misspelled commands
setopt   INC_APPEND_HISTORY  # Append history file immediately
setopt   SHARE_HISTORY       # Read history file for history
setopt   HIST_IGNORE_SPACE
# HIST_REDUCE_BLANKS corrupts history.
# https://bugs.launchpad.net/ubuntu/+source/zsh/+bug/1334858
unsetopt   HIST_REDUCE_BLANKS
setopt   HIST_IGNORE_ALL_DUPS
setopt   EXTENDED_HISTORY
unsetopt BEEP                # No beeps on error
unsetopt HIST_BEEP           # No history beeps
unsetopt LIST_BEEP           # No list beeps
unsetopt CHASE_DOTS          # don't resolve .. in cd
unsetopt CHASE_LINKS         # don't resolve symbolic links in cd
setopt   AUTO_CD             # use 'cd x' if 'x' is run and is not a command
setopt   PUSHD_IGNORE_DUPS   # don't push multiple copies onto dir stack
setopt   PUSHD_SILENT        # don't print stack after pushd/popd
setopt   AUTO_PUSHD          # cd pushes dir on to dir stack
unsetopt NOMATCH             # no error if glob fails to expand (scp fix)
unsetopt FLOW_CONTROL        # turn off output flow control (so ^S/^Q work)

stty -ixon -ixoff 2>/dev/null # really, no flow control.
autoload colors
if [[ "$terminfo[colors]" -ge 8 ]]; then
  colors
fi

autoload -U compinit; compinit -d "${HOME}/.zsh/.zcompdump"
autoload -U age
autoload -U zmv

alias l='ls'
alias ll='ls -lh'
alias la='ls -a'
alias lla='ls -alh'
alias lsd='ls -ld *(-/DN)'
alias j='jobs -l'
alias p='ps -fu $USER'
alias h='history'
alias hd='od -Ax -tx1z -v'  # convenient hex dump

alias cp='nocorrect cp'
alias mv='nocorrect mv'
alias rm='nocorrect rm -i'
alias mkdir='nocorrect mkdir'

alias grep='grep --color -I'

# Editor preferences
alias emacs='emacs -nw'
export EDITOR='emacs'
export FCEDIT='emacs'

# Load oh-my-zsh plugins.
if [[ -d "${HOME}/.zsh/omz-plugins" ]]; then
  for file in ${HOME}/.zsh/omz-plugins/**/*(N.x); do
    source $file
  done
fi

# Load custom modules. Load these after oh-my-zsh so that customizations can be
# done here.
if [[ -d "${HOME}/.zsh" ]] ; then
  for file in "${HOME}"/.zsh/*(N.x:t) ; do
    source "${HOME}/.zsh/${file}"
  done
fi
