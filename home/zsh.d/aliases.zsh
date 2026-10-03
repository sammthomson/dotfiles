# cd
alias ..='cd ..'

# mkdir
alias mkdir="mkdir -p"

# emacs
alias e="emacs"
alias et="emacs -nw"

# git
alias gti="git"
alias gs="git status"

alias reload="source ~/.zshrc"

if (( $+commands[nmcli] )); then
  alias wakeup="sudo nmcli nm sleep false"
fi

tgz() {
  (( $# >= 2 )) || {
    print -u2 "Usage: tgz ARCHIVE PATH..."
    return 2
  }
  local archive=$1
  shift
  tar -czf "$archive" "$@"
}

alias tunnel="ssh -C2qTnN -D 6789"

bak() {
  (( $# == 1 )) || {
    print -u2 "Usage: bak PATH"
    return 2
  }
  [[ ! -e "$1.bak" ]] || {
    print -u2 "Backup already exists: $1.bak"
    return 1
  }
  mv -- "$1" "$1.bak"
}
