# To install using rosetta, first start a new Rosetta shell with
#  `arch -x86_64 /bin/zsh`
# Then run this script from that shell.

# homebrew
if [ -z $(which brew) ]
then
	/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi
brew doctor

# brew install bash
# NEW_BASH='/usr/local/bin/bash'
# grep -qF ${NEW_BASH} /etc/shells || echo ${NEW_BASH} | sudo tee -a /etc/shells
brew install zsh
export NEW_ZSH='/usr/local/bin/zsh'
grep -qF ${NEW_ZSH} /etc/shells || echo ${NEW_ZSH} | sudo tee -a /etc/shells
sudo ln -s /etc/zshenv /etc/zprofile # OSX workaround
chsh -s "${NEW_ZSH}"


brew install coreutils
brew install findutils
brew install grep
brew install gnu-indent
brew install gnu-sed
brew install gnutls
brew install grep
brew install gnu-tar
brew install gawk
brew install wget
brew install readline
brew install emacs
brew services start emacs

brew install pkg-config gmp libffi boehmgc llvm

# Python:
brew install openssl readline sqlite3 xz zlib  # pyenv dependencies
brew install pyenv
pyenv install 3.7.13  # for pyharbor
pyenv install 3.9.14  # for harbor
pyenv install 3.10.6  # latest stable as of 10/5/2022
pyenv global 3.7.13
alias python="$(pyenv which python)"
alias pip="$(pyenv which pip)"
pip install --upgrade pip
pip install virtualenv
curl -sSL https://install.python-poetry.org | POETRY_VERSION=1.1.15 python -
pip install ipython  # i like to have this globally
alias ipy="$(pyenv which ipython)"


# Node:
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
nvm install v16.18.0

# JVM:
curl -s "https://get.sdkman.io" | bash
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
sdk install java 11.0.16.1-tem
sdk install sbt 1.7.2
sdk install scala 2.13.8


brew install aspell
brew install automake
brew install cmake
brew install make
brew install htop
brew install tmux
brew install watch
brew install git
# brew install mercurial
brew install tree
brew install ack
brew install trash
brew install rename
brew install ffmpeg
brew install graphicsmagick
brew install chrome-cli
brew install starship
# brew install --cask java6
# brew install --cask java7
# brew install --cask java # 8
# brew install maven
# brew install scala
brew install coursier/formulas/coursier
cs setup
brew install --cask iterm2

# Install community editions of these by hand:
# brew install --cask intellij-idea
# brew install --cask pycharm

# brew install --cask eclipse-ide
brew install --cask docker
brew install docker-compose
brew install docker-machine

brew install jq

brew install macfuse
# brew install sshfs # has to come after macfuse
brew install --cask google-chrome
brew install --cask firefox
brew install --cask sublime-text
brew install --cask skim
brew install --cask cyberduck # ftp
brew install --cask zotero
# brew install --cask r
# # haskell
# brew install --cask haskell-platform
# cabal install cabal-install # upgrade cabal
# # idris
# cabal install idris
# latex
brew install --cask mactex

# window manager
# brew install --cask spectacle
# brew install --cask slate
brew install --cask rectangle
# to remap caps lock
# see https://msol.io/blog/tech/work-more-efficiently-on-your-mac-for-developers/
# brew install --cask seil
# brew install --cask karabiner
brew install --cask hyperkey


# brew install --cask spotify
brew install --cask vlc   # media player
# brew install --cask transmission  # brew says this is deprecated?
brew install transmission-cli  # bittorrent
# brew install --cask adium
# brew install --cask dropbox
# brew install --cask skype
brew install --cask insomnia
brew install --cask freedom
# brew install inkscape  # i don't usually install this until i need it
# anaconda?
brew install --cask microsoft-outlook
brew install --cask microsoft-teams
brew install --cask microsoft-excel
brew install --cask microsoft-word
brew install --cask microsoft-powerpoint
brew install --cask visual-studio-code


brew install discord

# # fonts (optional)
# brew tap homebrew/cask-fonts
# brew install --cask font-hack-nerd-font

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
