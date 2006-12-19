# $Id$
#
# Matt Sparks (f0rked)

### Set options
setopt   CORRECT                             # correct misspelled commands
unsetopt BEEP                                # No beeps on error
#setopt   PROMPT_SUBST                       # Prompt substitution/expansion

bindkey -e                                   # Use emacs keybindings

### Set up completion
autoload -U compinit
compinit

### Set up (Gentoo-style) prompt
if ((EUID == 0)); then
    PROMPT=$'%{\e[01;31m%}%m %{\e[01;34m%}%~ %# %{\e[00m%}'
else
    PROMPT=$'%{\e[01;32m%}%n@%m %{\e[01;34m%}%~ %# %{\e[00m%}'
fi

### Change names of screen windows to running program
precmd() {
    if [[ "$STY" != "" ]]; then
        echo -ne "\ekzsh\e\\"
    fi
}
preexec() {
    if [[ "$STY" != "" ]]; then
        local CMD=`echo $1 | sed 's/^sudo //; s/ .*//'`
        echo -ne "\ek$CMD\e\\"
    fi
}

### Change the window title of X terminals 
case $TERM in
    *xterm*|rxvt|(dt|k|E|a)term)
        precmd() {
            print -Pn "\e]0;%n@%m %~\a"
        }
        preexec() {
            print -Pn "\e]0;%n@%m <$1> %~\a"
        }
    ;;
esac

### Colors for lists
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

### Add hostname completion for hosts in ~/.ssh/known_hosts
local _myhosts
_myhosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*} )
zstyle ':completion:*' hosts $_myhosts

### Keybindings 
case $TERM in
    linux|screen)
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
