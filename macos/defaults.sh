#!/bin/sh

set -eu

[ "$(uname -s)" = "Darwin" ] || {
  echo "This script supports macOS only." >&2
  exit 1
}

# Expand save and print panels by default.
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true

# Prefer local, literal text entry without automatic substitutions.
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false
defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false
defaults write NSGlobalDomain NSTextShowsControlCharacters -bool true

# Input behavior.
defaults write NSGlobalDomain com.apple.trackpad.scaling -float 2
defaults write NSGlobalDomain com.apple.mouse.scaling -float 2.5
defaults write com.apple.BezelServices kDimTime -int 300

# Require a password immediately after sleep or screen saver activation.
defaults write com.apple.screensaver askForPassword -int 1
defaults write com.apple.screensaver askForPasswordDelay -int 0

# Avoid metadata files on network volumes and accidental Chrome backswipes.
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
defaults write com.google.Chrome AppleEnableSwipeNavigateWithScrolls -bool false
defaults write com.google.Chrome.canary AppleEnableSwipeNavigateWithScrolls -bool false

echo "macOS defaults applied. Some changes require restarting affected apps."
