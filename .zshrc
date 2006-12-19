# $Id$
#
# Matt Sparks (f0rked)

### Set options
setopt   CORRECT                             # correct misspelled commands
unsetopt BEEP                                # No beeps on error
setopt   EMACS                               # Emacs keybindings for ZLE
setopt   PROMPT_SUBST                        # Prompt substitution/expansion

### Set up (Gentoo-style) prompt
PROMPT=$'%{\e[01;32m%}%n@%m %{\e[01;34m%}%~ %# %{\e[00m%}'

### Various things for coloring outputs
if echo hello|grep --color=auto l >/dev/null 2>&1; then
    export GREP_OPTIONS='--color=auto' GREP_COLOR='1;32'
fi

if which dircolors >&/dev/null; then
    if [[ -e "${zdotdir}/.dircolors" ]]; then
        eval `dircolors -b $zdotdir/.dircolors`
    else
        eval `dircolors -b`
    fi
fi

if [[ $ZSH_VERSION > 3.1.5 ]]; then
    zmodload -i zsh/complist
    
    zstyle ':completion:*' list-colors ''
    
    zstyle ':completion:*:*:kill:*:processes' list-colors \
        '=(#b) #([0-9]#)*=0=01;31'
    
    zstyle ':completion:*' list-colors "$LS_COLORS"
fi  

### Other aliases
alias sshg="ssh -X f0rked@godfather.f0rked.com"
alias l='ls'
alias ll='ls -lh'
alias la='ls -a'
alias ls='ls --color=auto'
alias lla='ls -alh'
alias lsd='ls -ld *(-/DN)'
alias j='jobs -l'
alias p='ps -fu $USER'
alias h='history'
alias du='du -h'
alias df='df -h'

### Change the window title of X terminals 
case $TERM in
    *xterm*|rxvt|(dt|k|E|a)term)
        precmd() {
            print -Pn "\033]0;%n@%m %~\007"
        }
        preexec() {
            print -Pn "\033]0;%n@%m <$1> %~\007"
        }
    ;;
esac

### Add hostname completion for hosts in ~/.ssh/known_hosts
local _myhosts
_myhosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*} )
zstyle ':completion:*' hosts $_myhosts

### Keybindings 
case $TERM in
    linux)
        bindkey "^[[2~" yank                 # Insert
        bindkey "^[[3~" delete-char          # Delete
        bindkey "^[[5~" up-line-or-history   # Page Up
        bindkey "^[[6~" down-line-or-history # Page Down
        bindkey "^[[1~" beginning-of-line    # Home
        bindkey "^[[4~" end-of-line          # End
        bindkey "^[e" expand-cmd-path        # Meta+E
        bindkey "^[[A" up-line-or-search
        bindkey "^[[B" down-line-or-search
        bindkey " " magic-space
    ;;
    
    *xterm*|rxvt|(dt|k|a|E)term)
        bindkey "^[[2~" yank                 # Insert
        bindkey "^[[3~" delete-char          # Delete
        bindkey "^[[5~" up-line-or-history   # Page Up
        bindkey "^[[6~" down-line-or-history # Page Down
        bindkey "^[[7~" beginning-of-line    # Home
        bindkey "^[[8~" end-of-line          # End
        bindkey "^[e" expand-cmd-path        # Meta+E
        bindkey "^[[A" up-line-or-search
        bindkey "^[[B" down-line-or-search
        bindkey " " magic-space
    ;;
esac

### Environment variables
export EDITOR=`which emacs`
export FCEDIT=`which emacs`

### Shell variables
HISTFILE=$HOME/.zhistory                     # history file name
SAVEHIST=5000                                # lines of history

### Watch settings
watch=()                                     # watch for login/logout events
LOGCHECK=30                                  # seconds between checks
WATCHFMT='%n %a %l from %m at %T'            # format for printing events
