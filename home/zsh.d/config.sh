export EDITOR='emacs -nw'
# make sure emacs has somewhere to put *~ backup files
export EMACS_BACKUP_DIR="~/.emacs.d/backups"
if [ -d ${EMACS_BACKUP_DIR} ]; then
        mkdir ${EMACS_BACKUP_DIR}
fi

# Ctr-arrow for moving by word in terminal
# bind '"\e[5C": forward-word'
# bind '"\e[5D": backward-word'
# bind '"\e[1;5C": forward-word'
# bind '"\e[1;5D": backward-word'

export PATH="$PATH:$HOME/bin:$HOME/.cabal/bin"
export PATH="/opt/scala/current/bin:$PATH"


# show the current git branch
#export PS1='[\u@\h \W$(__git_ps1 " (%s)")]\$ '

## from http://www.ukuug.org/events/linux2003/papers/bash_tips/
## shared history
#shopt -s histappend
#export PROMPT_COMMAND='history -a'

#shopt -s cdspell

#export CDPATH='~/links'

export IGNOREEOF=1
