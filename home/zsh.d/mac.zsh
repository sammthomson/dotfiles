[[ "$OSTYPE" == darwin* ]] || return 0
(( $+commands[brew] )) || return 0

# use gnu utils instead of darwin knock-offs
toolnames=(
  "coreutils"
  "make"
)
for toolname in $toolnames
do
  prefix="$(brew --prefix "${toolname}" 2>/dev/null)" || continue
  export PATH="${prefix}/libexec/gnubin:${PATH}"
  export MANPATH="${prefix}/libexec/gnuman:${MANPATH}"
done

# alias git=hub

# put `subl` on the path
export PATH="/Applications/Sublime Text.app/Contents/SharedSupport/bin:$PATH"

alias top='sudo htop'
