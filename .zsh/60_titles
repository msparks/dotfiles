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