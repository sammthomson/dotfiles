export JAVA_HOME_7="/Library/Java/JavaVirtualMachines/jdk1.7.0_76.jdk/Contents/Home/"
export JAVA_HOME=${JAVA_HOME_7}

export PATH="$HOME/Library/Haskell/bin:$PATH"
# Setting PATH for Python 2.7
export PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}"
# use gnu utils instead of darwin knock-offs
export PATH="$(brew --prefix coreutils)/libexec/gnubin:${PATH}"
export MANPATH="$(brew --prefix coreutils)/libexec/gnuman:${MANPATH}"
# alias git=hub

# Anaconda
export PATH="/opt/anaconda/bin:$PATH"

export PATH="$HOME/Library/Haskell/bin:$PATH"

# Homebrew turned evil
HOMEBREW_NO_ANALYTICS=1