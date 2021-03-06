# homebrew
if [ -z `which brew` ]
then
	ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi
brew doctor

# brew install bash
# NEW_BASH='/usr/local/bin/bash'
# grep -qF ${NEW_BASH} /etc/shells || echo ${NEW_BASH} | sudo tee -a /etc/shells
brew install zsh
NEW_ZSH='/usr/local/bin/zsh'
grep -qF ${NEW_ZSH} /etc/shells || echo ${NEW_ZSH} | sudo tee -a /etc/shells
sudo ln -s /etc/zshenv /etc/zprofile # OSX workaround
chsh -s /bin/zsh
brew install coreutils
brew install findutils
brew tap homebrew/dupes
brew install homebrew/dupes/grep
brew install gnu-indent
brew install gnu-sed
brew install gnutls
brew install grep
brew install gnu-tar
brew install gawk
brew install wget
brew install readline
brew services start emacs
brew install aspell
brew install automake
brew install cmake
brew install htop
brew install tmux
brew install watch
brew install git
brew install mercurial
brew install tree
brew install ack
brew install trash
brew install rename
brew install ffmpeg
brew install graphicsmagick
brew install chrome-cli
brew install caskroom/cask/brew-cask
brew tap caskroom/versions
# brew cask install java6
# brew cask install java7
brew cask install java # 8
brew install maven
brew install scala
brew install sbt
brew install --HEAD paulp/extras/coursier  # better dependency resolution than ivy
brew cask install iterm2
brew cask install intellij-idea
# brew cask install eclipse-ide
brew cask install docker
brew install docker-compose
brew install docker-machine
brew cask install osxfuse
brew install sshfs # has to come after osxfuse
brew cask install google-chrome
brew cask install firefox
brew cask install sublime-text
brew cask install skim
brew cask install cyberduck # ftp
brew cask install zotero
# brew cask install r
# # haskell
# brew cask install haskell-platform
# cabal install cabal-install # upgrade cabal
brew install pkg-config gmp libffi boehmgc llvm
# # idris
# cabal install idris
# latex
brew cask install mactex
# # trying out slate instead
# brew cask install spectacle
brew cask install slate
# to remap caps lock
# see https://msol.io/blog/tech/work-more-efficiently-on-your-mac-for-developers/
brew cask install seil
brew cask install karabiner
# brew cask install spotify
brew cask install vlc
brew cask install transmission
# brew cask install adium
# brew cask install dropbox
# brew cask install skype
brew cask install flash-player
brew cask install silverlight
brew cask install insomnia
# brew install inkscape  # i don't usually install this until i need it
# anaconda?
brew install starship

brew cleanup



# powerline fonts
cd ~/code && git clone https://github.com/powerline/fonts.git --depth=1 && cd fonts && ./install.sh && cd .. && rm -rf fonts


# Mac OS defaults
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true
defaults write NSGlobalDomain NSTextShowsControlCharacters -bool true
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1
defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false
defaults write -g com.apple.trackpad.scaling 2
defaults write -g com.apple.mouse.scaling 2.5
# Turn off keyboard illumination when computer is not used for 5 minutes
defaults write com.apple.BezelServices kDimTime -int 300
# Requiring password immediately after sleep or screen saver begins
defaults write com.apple.screensaver askForPassword -int 1
defaults write com.apple.screensaver askForPasswordDelay -int 0
# Avoid creation of .DS_Store files on network volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
# Privacy: Don’t send search queries to Apple
defaults write com.apple.Safari UniversalSearchEnabled -bool false
defaults write com.apple.Safari SuppressSearchSuggestions -bool true
echo "Disabling the annoying backswipe in Chrome"
defaults write com.google.Chrome AppleEnableSwipeNavigateWithScrolls -bool false
defaults write com.google.Chrome.canary AppleEnableSwipeNavigateWithScrolls -bool false

# Speeding up wake from sleep to 24 hours from an hour
# http://www.cultofmac.com/221392/quick-hack-speeds-up-retina-macbooks-wake-from-sleep-os-x-tips/
sudo pmset -a standbydelay 86400
