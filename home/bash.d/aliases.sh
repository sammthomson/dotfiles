# cd
alias ..='cd ..'

# ls
alias l="ls -lah"
alias ll="ls -l"
alias la='ls -A'

# mkdir
alias mkdir="mkdir -p"

# emacs
alias e="emacs"
alias et="emacs -nw"

# grep
#alias grep="grep -r"

# git
alias gti="git"
alias gl='git pull'
alias gp='git push'
alias gd='git diff'
alias gc='git commit'
alias gca='git commit -a'
alias gco='git checkout'
alias gb='git branch'
alias gs='git status'
alias grm="git status | grep deleted | awk '{print \$3}' | xargs git rm"
alias changelog='git log `git log -1 --format=%H -- CHANGELOG*`..; cat CHANGELOG*'

alias reload='. ~/.bash_profile'

alias wakeup='sudo nmcli nm sleep false'

alias tgz='tar -cvzf'

# util
backupFile() {
    mv $1{,.bak}
}
alias bak=backupFile
