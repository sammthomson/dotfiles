#!/bin/sh

set -eu

mode=${1:-}
case "$mode" in
  --check|--link|--backup)
    ;;
  *)
    echo "Usage: $0 --check|--link|--backup" >&2
    exit 2
    ;;
esac

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$script_dir
timestamp=$(date +%Y%m%d%H%M%S)
status=0

info() {
  printf '%s\n' "$*"
}

fail() {
  printf 'Error: %s\n' "$*" >&2
  exit 1
}

backup_path() {
  printf '%s.backup.%s\n' "$1" "$timestamp"
}

link_matches() {
  [ -L "$2" ] && [ "$(readlink "$2")" = "$1" ]
}

install_link() {
  source_path=$1
  target_path=$2

  if link_matches "$source_path" "$target_path"; then
    info "ok: $target_path"
    return
  fi

  if [ "$mode" = "--check" ]; then
    info "missing or different: $target_path"
    status=1
    return
  fi

  if [ -e "$target_path" ] || [ -L "$target_path" ]; then
    if [ "$mode" != "--backup" ]; then
      fail "$target_path already exists; rerun with --backup to preserve it"
    fi
    destination=$(backup_path "$target_path")
    [ ! -e "$destination" ] || fail "backup already exists: $destination"
    mv "$target_path" "$destination"
    info "backed up: $target_path -> $destination"
  fi

  mkdir -p "$(dirname -- "$target_path")"
  ln -s "$source_path" "$target_path"
  info "linked: $target_path -> $source_path"
}

install_zsh_loader() {
  target_path=$HOME/.zshrc
  source_path=$repo_root/home/zshrc
  managed_start="# >>> sammthomson dotfiles >>>"
  managed_end="# <<< sammthomson dotfiles <<<"
  escaped_source=$(printf '%s' "$source_path" | sed 's/[\\`"$]/\\&/g')
  source_line="source \"$escaped_source\""

  if [ -f "$target_path" ] &&
     grep -Fqx "$managed_start" "$target_path" &&
     grep -Fqx "$source_line" "$target_path" &&
     grep -Fqx "$managed_end" "$target_path"; then
    info "ok: $target_path"
    return
  fi

  if [ "$mode" = "--check" ]; then
    info "missing or outdated loader: $target_path"
    status=1
    return
  fi

  if link_matches "$source_path" "$target_path"; then
    rm "$target_path"
    info "removed legacy symlink: $target_path"
  elif [ -L "$target_path" ]; then
    if [ "$mode" != "--backup" ]; then
      fail "$target_path is a different symlink; rerun with --backup"
    fi
    destination=$(backup_path "$target_path")
    mv "$target_path" "$destination"
    info "backed up: $target_path -> $destination"
  elif [ -f "$target_path" ] && [ "$mode" = "--backup" ]; then
    destination=$(backup_path "$target_path")
    cp -p "$target_path" "$destination"
    info "backed up: $target_path -> $destination"
  elif [ -e "$target_path" ] && [ ! -f "$target_path" ]; then
    fail "$target_path exists and is not a regular file"
  fi

  temporary=$(mktemp "${target_path}.tmp.XXXXXX")
  if [ -f "$target_path" ]; then
    sed "/^${managed_start}$/,/^${managed_end}$/d" "$target_path" > "$temporary"
  fi

  if [ -s "$temporary" ]; then
    printf '\n' >> "$temporary"
  fi
  {
    printf '%s\n' "$managed_start"
    printf '%s\n' "$source_line"
    printf '%s\n' "$managed_end"
  } >> "$temporary"
  mv "$temporary" "$target_path"
  info "installed loader: $target_path"
}

ensure_git_config() {
  key=$1
  value=$2

  if git config --global --get-all "$key" 2>/dev/null | grep -Fqx "$value"; then
    info "ok: git $key"
    return
  fi

  if [ "$mode" = "--check" ]; then
    info "missing: git $key = $value"
    status=1
    return
  fi

  git config --global --add "$key" "$value"
  info "configured: git $key"
}

prepare_git_config() {
  target_path=$HOME/.gitconfig
  legacy_source=$repo_root/home/gitconfig

  if ! link_matches "$legacy_source" "$target_path"; then
    return
  fi

  if [ "$mode" = "--check" ]; then
    info "legacy symlink: $target_path -> $legacy_source"
    status=1
    return
  fi

  if [ "$mode" != "--backup" ]; then
    fail "$target_path is a legacy symlink; rerun with --backup to preserve it"
  fi

  destination=$(backup_path "$target_path")
  [ ! -e "$destination" ] && [ ! -L "$destination" ] ||
    fail "backup already exists: $destination"

  if [ -e "$target_path" ]; then
    cp -p "$target_path" "$destination"
    rm "$target_path"
  else
    mv "$target_path" "$destination"
  fi
  info "backed up legacy symlink: $target_path -> $destination"
}

[ -n "${HOME:-}" ] || fail "HOME is not set"
command -v git >/dev/null 2>&1 || fail "git is required"

install_zsh_loader
install_link "$repo_root/home/emacs.d" "$HOME/.emacs.d"
install_link "$repo_root/home/ghc" "$HOME/.ghc"
install_link "$repo_root/home/inputrc" "$HOME/.inputrc"
install_link "$repo_root/home/pylintrc" "$HOME/.pylintrc"
install_link "$repo_root/mise.toml" "$HOME/.config/mise/config.toml"

prepare_git_config
ensure_git_config "include.path" "$repo_root/git/common.gitconfig"
personal_root="$HOME/code/sammthomson/"
ensure_git_config "includeIf.gitdir/i:$personal_root.path" \
  "$repo_root/git/personal.gitconfig"

exit "$status"
