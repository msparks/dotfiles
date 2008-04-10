# ~/.zshrc
# $Date$
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

### Change the titles of X terminals and screen windows
case $TERM in
    *xterm*|rxvt|(dt|k|E|a)term)
        precmd() {
            print -Pn "\e]0;%n@%m %~\a"
        }
        preexec() {
            print -Pn "\e]0;%n@%m <${(Vq)2}> %~\a"
        }
    ;;
    screen)
        precmd() {
            print -Pn "\ekzsh\e\\"
        }
        preexec() {
            local CMD=`echo $1 | sed 's/^sudo //; s/ .*//' | head -n 1`
            print -Pn "\ek$CMD\e\\"
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

### More completions
# Add hostname completion for hosts in ~/.ssh/known_hosts
local _myhosts
_myhosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*} )
zstyle ':completion:*' hosts $_myhosts

# Ignore what's already on the line for certain commands
zstyle ':completion:*:(rm|kill|diff):*' ignore-line yes

# Ignore parent directory
zstyle ':completion:*:(cd|mv|cp):*' ignore-parents parent pwd

# Ignore uninteresting users for the user completion (for chmod, etc)
zstyle ':completion:*:*:*:users' ignored-patterns \
    adm apache bin daemon games gdm halt ident junkbust lp mail mailnull \
    named news nfsnobody nobody nscd ntp operator pcap postgres radvd \
    rpc rpcuser rpm shutdown squid sshd sync uucp vcsa xfs backup  bind  \
    dictd  gnats  identd  irc  man  messagebus  postfix  proxy  sys  www-data

# Complete on the current prefix (before the cursor) ignoring the suffix with ^i
bindkey '^i' expand-or-complete-prefix

### Other aliases
alias ssht="ssh -X f0rked.com"

case $HOME in
  /Users/*)
    alias ls="gls --color=auto"
    alias du="gdu -h"
    alias df="gdf -h"
    alias find="gfind"
  ;;

  *)
    alias ls="ls --color=auto"
    alias du='du -h'
    alias df='df -h'
  ;;
esac

alias l='ls'
alias ll='ls -lh'
alias la='ls -a'
alias lla='ls -alh'
alias lsd='ls -ld *(-/DN)'
alias j='jobs -l'
alias p='ps -fu $USER'
alias h='history'

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

# For F keys
bindkey "^[[13~" digit-argument
bindkey "^[[14~" digit-argument
bindkey "^[[15~" digit-argument
bindkey "^[[16~" digit-argument
bindkey "^[[17~" digit-argument

# Turn off terminal driver flow control
stty -ixon -ixoff

### Environment variables
export EDITOR=`which vim`
export FCEDIT=`which vim`
export LESS="-R -M --shift 5"
export LESSOPEN="|lesspipe.sh %s"
export MANPATH="$MANPATH:/opt/local/share/man"

# Add to and unique-ify the path
path=($path /usr/sbin /sbin ~/bin /opt/bin /usr/local/sbin /usr/local/bin)
path=($path /opt/local/bin /opt/local/sbin)  # DarwinPorts paths
typeset -U path

### Shell variables
HISTFILE=$HOME/.zhistory                     # history file name
SAVEHIST=5000                                # lines of history

### Watch settings
watch=()                                     # watch for login/logout events
LOGCHECK=30                                  # seconds between checks
WATCHFMT='%n %a %l from %m at %T'            # format for printing events

### Local settings
touch ~/.zshrc.local
source ~/.zshrc.local
