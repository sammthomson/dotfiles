#!/bin/zsh

set -euo pipefail

repo_root="${0:A:h:h}"

[[ "$(uname -s)" == "Darwin" ]] ||
  { print -u2 "This bootstrap supports macOS only."; exit 1; }
[[ "$(/usr/bin/uname -m)" == "arm64" ]] ||
  { print -u2 "This bootstrap supports Apple Silicon only."; exit 1; }

if [[ ! -x /opt/homebrew/bin/brew ]]; then
  /bin/bash -c \
    "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

eval "$(/opt/homebrew/bin/brew shellenv)"

brew bundle --no-upgrade --file="$repo_root/Brewfile"
"$repo_root/deploy.sh" --link
mise install
sh "$repo_root/macos/defaults.sh"

print "macOS bootstrap complete. Start a new shell to load the tracked profile."
