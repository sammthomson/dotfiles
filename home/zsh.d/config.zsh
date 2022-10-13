export EDITOR='emacs -nw'
# make sure emacs has somewhere to put *~ backup files
export EMACS_BACKUP_DIR="~/.emacs.d/backups"
if [ -d ${EMACS_BACKUP_DIR} ]; then
        mkdir ${EMACS_BACKUP_DIR}
fi


# https://superuser.com/a/357394

bindkey -e
# bindkey '\e[A' history-beginning-search-backward
# bindkey '\e[B' history-beginning-search-forward

# bindkey '^[[1;9A' history-search-backward
bindkey '\e[A' history-search-backward
bindkey '\e[B' history-search-forward


# Make sure to unbind these in Settings > Keyboard > Mission Control > Move {left/right} a space
bindkey '\e\e[C' forward-word     # [Ctrl-RightArrow] - move forward one word
bindkey '\e\e[D' backward-word    # [Ctrl-LeftArrow] - move backward one word
# bindkey '^[[1;5C' forward-word     # [Ctrl-RightArrow] - move forward one word
# bindkey '^[[1;5D' backward-word    # [Ctrl-LeftArrow] - move backward one word


# For user-local installed binaries
export PATH="${HOME}/.local/bin:${PATH}"


## from http://www.ukuug.org/events/linux2003/papers/bash_tips/
## shared history
#shopt -s histappend
#export PROMPT_COMMAND='history -a'

#shopt -s cdspell

#export CDPATH='~/links'

export IGNOREEOF=1
