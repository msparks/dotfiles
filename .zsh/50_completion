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