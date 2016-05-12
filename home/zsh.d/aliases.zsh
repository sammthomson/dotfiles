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
grm() {
  git rm $(git ls-files --deleted)
}
# gs got clobbered by 'git status'
alias ghostscript="/usr/local/bin/gs"

alias reload="source ~/.zshrc"

# some kerberos thing or sth? i forget
alias wakeup="sudo nmcli nm sleep false"

alias tgz="tar -cvzf"

bak() {
  mv $1{,.bak}
}
