# not currently using
export ANACONDA_BIN="/usr/local/anaconda3/bin"
# export PATH="${ANACONDA_BIN}:$PATH"

export WORKON_HOME="${HOME}/code/.virtualenvs"
export PROJECT_HOME="${HOME}/code"

for venvdir in "/usr/local/bin" "/Library/Frameworks/Python.framework/Versions/2.7/bin" "${ANACONDA_BIN}"; do
    if [ -e ${venvdir}/virtualenvwrapper.sh ]; then
        source "${venvdir}/virtualenvwrapper.sh"
    fi
done

export PATH="$PATH:$HOME/.pyenv/shims"


# azure-cli for harbor
alias az="~/.pyenv/versions/3.9.14/bin/az"


alias ipy="ipython"
