# export JAVA_HOME_7="/Library/Java/JavaVirtualMachines/jdk1.7.0_76.jdk/Contents/Home/"
# export JAVA_HOME=${JAVA_HOME_7}

# export PATH="$HOME/Library/Haskell/bin:$PATH"

# use gnu utils instead of darwin knock-offs
toolnames=(
  "coreutils"
  "make"
)
for toolname in $toolnames
do
  prefix="$(brew --prefix ${toolname})"
  export PATH="${prefix}/libexec/gnubin:${PATH}"
  export MANPATH="${prefix}/libexec/gnuman:${MANPATH}"
done

# alias git=hub

# Homebrew turned evil
HOMEBREW_NO_ANALYTICS=1

# put `subl` on the path
export PATH="/Applications/Sublime Text.app/Contents/SharedSupport/bin:$PATH"

alias top='sudo htop'
