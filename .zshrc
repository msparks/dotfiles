# ~/.zshrc
# Matt Sparks
# quadpoint.org
#
# Considerable zsh-fu from compnerd, jdong, Mako, and majnematic.

# History variables
HISTSIZE=1000
HISTFILE=~/.zsh/.history     # history file name
SAVEHIST=1000                # lines of history

# Set options
setopt   CORRECT             # correct misspelled commands
setopt   INC_APPEND_HISTORY  # Append history file immediately
setopt   SHARE_HISTORY       # Read history file for history
setopt   HIST_IGNORE_SPACE
setopt   HIST_REDUCE_BLANKS
setopt   HIST_IGNORE_ALL_DUPS
unsetopt EXTENDED_HISTORY
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
stty -ixon -ixoff            # really, no flow control.

bindkey -e                   # Use emacs keybindings

autoload -U compinit; compinit -d "${HOME}/.zsh/.zcompdump"
autoload -U age
autoload -U zmv

# Set up (Gentoo-style) prompt
if [[ $EUID == "0" ]]; then
  PROMPT=$'%{\e[01;31m%}%m %{\e[01;34m%}%~ %(?..%{\e[01;31m%})%(!..)%# %{\e[00m%}'
else
  PROMPT=$'%{\e[01;32m%}%n@%m %{\e[01;34m%}%~ %(?..%{\e[01;31m%})%(!.#.)%# %{\e[00m%}'
fi

# Change the titles of X terminals and screen windows
case $TERM in
  *xterm*|rxvt|(dt|k|E|a)term)
    precmd() { print -Pn "\e]0;%n@%m %~\a" }
    preexec() { print -Pn "\e]0;%n@%m <${(Vq)2}> %~\a" }
  ;;
  screen*)
    precmd() { print -Pn "\ekzsh\e\\" }
    preexec() {
      local CMD=`echo $1 | sed 's/^sudo //; s/ .*//' | head -n 1`
      print -Pn "\ek$CMD\e\\"
    }
  ;;
esac

# Colors for lists
if which dircolors >&/dev/null; then
  if [[ -e "${zdotdir}/.dircolors" ]]; then
    eval `dircolors -b $zdotdir/.dircolors`
  else
    eval `dircolors -b`
  fi
fi

# Completions
zstyle ':completion:*' list-colors ''

# Caching
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "${HOME}/.zsh/.${HOST}-cache"

# add colors to processes for kill completion
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt \
  '%SAt %p: Hit TAB for more, or the character to insert%s'

zstyle ':completion:*' menu select=1 _complete _ignored _approximate
zstyle ':completion:*:*:kill:*:processes' command 'ps --forest -A -o pid,user,cmd'
zstyle ':completion:*:processes-names' command 'ps axho command'

# Add hostname completion for hosts in ~/.ssh/known_hosts
local _myhosts
_myhosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*} )
zstyle ':completion:*' hosts $_myhosts

# Allow one error for every three characters typed in approximate completer
zstyle -e ':completion:*:approximate:*' max-errors \
  'reply=( $(( ($#PREFIX+$#SUFFIX)/2 )) numeric )'

zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'

# Insert all expansions for expand completer
zstyle ':completion:*:expand:*' tag-order all-expansions

# List of completers to use
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate

# Formatting and messages
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''

# Match uppercase from lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# Ignore what's already on the line for certain commands
zstyle ':completion:*:(rm|kill|diff):*' ignore-line yes

# Ignore parent directory
zstyle ':completion:*:(cd|mv|cp):*' ignore-parents parent pwd

# Ignore completion functions (until the _ignored completer)
zstyle ':completion:*:functions' ignored-patterns '_*'

# Ignore uninteresting users for the user completion (for chmod, etc)
zstyle ':completion:*:*:*:users' ignored-patterns \
  adm apache bin daemon games gdm halt ident junkbust lp mail mailnull \
  named news nfsnobody nobody nscd ntp operator pcap postgres radvd \
  rpc rpcuser rpm shutdown squid sshd sync uucp vcsa xfs backup bind \
  dictd gnats identd irc man messagebus postfix proxy sys www-data

# Filename suffixes to ignore during completion (except after rm command)
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*?.o' '*?.c~' \
  '*?.old' '*?.pro'

# SSH Completion
zstyle ':completion:*:scp:*' tag-order \
  files users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
zstyle ':completion:*:scp:*' group-order \
  files all-files users hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order \
  users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
zstyle ':completion:*:ssh:*' group-order \
  hosts-domain hosts-host users hosts-ipaddr
zstyle '*' single-ignored show

# Complete on the current prefix (before the cursor) ignoring the suffix with ^i
bindkey '^i' expand-or-complete-prefix

# Add to and unique-ify the path
path=($path /usr/sbin /sbin ~/bin /opt/bin /usr/local/sbin /usr/local/bin)
path=($path /opt/local/bin /opt/local/sbin)  # MacPorts paths
typeset -U path

case `uname -s` in
  SunOS|Darwin|*BSD)  # Look for GNU utils on non-GNU systems
    if which gls >&/dev/null; then
      alias ls="gls --color=auto -F"
    else
      alias ls="ls -F"
    fi

    if which gdu >&/dev/null; then
      alias du="gdu -h"
    fi

    if which gdu >&/dev/null; then
      alias df="gdf -hT"
    else
      alias df="df -h"
    fi

    if which gfind >&/dev/null; then
      alias find="gfind"
    fi
  ;;

  *)
    alias ls="ls --color=auto -F"
    alias du='du -h'
    alias df='df -hT'
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
alias hd='od -Ax -tx1z -v'  # convenient hex dump

alias cp='nocorrect cp'
alias mv='nocorrect mv'
alias rm='nocorrect rm -i'
alias mkdir='nocorrect mkdir'

# reverse-sorted du with nice output
alias sdu="du -b | sort -rn | perl -ple 's|^(\d+)\s*(.*)$|sprintf qq(%9.2fM  \$2),(\$1/1024/1024)|e' | less"

# Color diffing
if which colordiff >&/dev/null; then
  alias diff='colordiff'
fi

# Keybindings
case $TERM in
  screen|xterm*|putty*)
    bindkey '\e[H' beginning-of-line
    bindkey '\e[F' end-of-line
    bindkey '\eOH' beginning-of-line
    bindkey '\eOF' end-of-line
    bindkey '\e[1~' beginning-of-line
    bindkey '\e[4~' end-of-line
    bindkey '\e[7~' beginning-of-line
    bindkey '\e[8~' end-of-line
    bindkey '\e[3~' delete-char
    bindkey '\e[1;5C' emacs-forward-word
    bindkey '\e[1;5D' emacs-backward-word
    bindkey '\e[5C' emacs-forward-word
    bindkey '\e[5D' emacs-backward-word
    bindkey '\eOC' emacs-forward-word
    bindkey '\eOD' emacs-backward-word
    bindkey '\eOc' emacs-forward-word
    bindkey '\eOd' emacs-backward-word
    bindkey '\e[c' emacs-forward-word
    bindkey '\e[d' emacs-backward-word
  ;;
  mlterm|kterm)
    bindkey '\e[H' beginning-of-line
    bindkey '\e[F' end-of-line
    bindkey '\e[1~' beginning-of-line
    bindkey '\e[4~' end-of-line
    bindkey '\e[1;5C' emacs-forward-word
    bindkey '\e[1;5D' emacs-backward-word
    bindkey '\e[3~' delete-char
  ;;
  linux|vt100)
    bindkey '\e[1~' beginning-of-line
    bindkey '\e[4~' end-of-line
    bindkey '\e[3~' delete-char
  ;;
  *rxvt*|Eterm|aterm)
    bindkey '\e[c' emacs-forward-word
    bindkey '\e[d' emacs-backward-word
    bindkey '\eOc' emacs-forward-word
    bindkey '\eOd' emacs-backward-word
    bindkey '\e[3~' delete-char
    bindkey '\e[7~' beginning-of-line
    bindkey '\e[8~' end-of-line
  ;;
  cons*)
    bindkey '\e[H' beginning-of-line
    bindkey '\e[F' end-of-line
    bindkey '^?' delete-char
  ;;
  interix)
    bindkey '\e[H' beginning-of-line
    bindkey '\e[U' end-of-line
    bindkey '^?' delete-char
  ;;
  sun*)
    bindkey '\e[214z' beginning-of-line
    bindkey '\e[220z' end-of-line
    bindkey '^?' delete-char
  ;;
  cygwin*)
    bindkey '\e[1~' beginning-of-line
    bindkey '\e[4~' end-of-line
  ;;
esac

# For F keys
bindkey "^[[13~" digit-argument
bindkey "^[[14~" digit-argument
bindkey "^[[15~" digit-argument
bindkey "^[[16~" digit-argument
bindkey "^[[17~" digit-argument

# Environment variables
export EDITOR=`which vim`
export FCEDIT=`which vim`
export LESS="-R -M --shift 5"
export LESSOPEN="|lesspipe.sh %s"
export MANPATH="$MANPATH:/opt/local/share/man"

# Set up vim for 256 colors if we might have support for it
#   This is necessary because screen fails.
if [[ $TERM == "xterm-256color" ||
      $TERM == "screen-bce" ||
      $TERM == "screen" ]]; then
  alias vim='vim -T xterm-256color'
fi

# Watch settings
watch=()                                     # watch for login/logout events
LOGCHECK=30                                  # seconds between checks
WATCHFMT='%n %a %l from %m at %T'            # format for printing events

# Extras
if [[ -d "${HOME}/.zsh" ]] ; then
  for file in "${HOME}"/.zsh/*(N.x:t) ; do
    source "${HOME}/.zsh/${file}"
  done
fi
