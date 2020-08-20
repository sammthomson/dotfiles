# export JAVA_HOME_7="/Library/Java/JavaVirtualMachines/jdk1.7.0_76.jdk/Contents/Home/"
# export JAVA_HOME=${JAVA_HOME_7}

# export PATH="$HOME/Library/Haskell/bin:$PATH"
# use gnu utils instead of darwin knock-offs
export PATH="$(brew --prefix coreutils)/libexec/gnubin:${PATH}"
export MANPATH="$(brew --prefix coreutils)/libexec/gnuman:${MANPATH}"
# alias git=hub


# Homebrew turned evil
HOMEBREW_NO_ANALYTICS=1

alias top='sudo htop'
