#!/bin/sh

echo "brew updating..."

brew update
outdated=`brew outdated`

if [ -n "$outdated" ]; then
	cat << EOF

The following package(s) will upgrade.

$outdated

Are you sure?
If you do not want to upgrade, please type Ctrl-c now.
EOF

	read dummy

	brew upgrade
fi

# Add Repository
brew tap homebrew/binary
brew tap hirocaster/homebrew-mozc-emacs-helper

# Memo:
# autoconf:   emacs with inline.patch needs autoheader
# pkg-config: for gem ref
# qt:         gem capybara-screenshot uses qmake
# readline:   for Ruby
# stunnel:    for mew
# webkit2png: % webkit2png -TF http://masutaka.net
brew install ack
brew install ansible
brew install aspell
brew install autoconf
brew install binutils
brew install boot2docker
brew install cmigemo
brew install docker
brew install git
brew install gnu-sed
brew install gnupg
brew install go
brew install heroku-toolbelt
brew install hub
brew install imagemagick
brew install jq
brew install jsl
brew install kindlegen
brew install lv
brew install markdown
brew install mercurial
brew install mozc-emacs-helper
brew install nginx
brew install nvm
brew install pkg-config
brew install plenv
brew install postgresql
brew install qt
brew install readline
brew install redis
brew install stunnel
brew install terminal-notifier
brew install tree
brew install unrar
brew install w3m
brew install webkit2png
brew install wget
brew install xz

brew install caskroom/cask/brew-cask
brew cask install appcleaner
brew cask install atom
brew cask install caffeine
brew cask install chromecast
brew cask install dropbox
brew cask install firefox
brew cask install flash
brew cask install flip4mac
brew cask install free-ruler
brew cask install github
brew cask install google-chrome --caskroom=/Applications
brew cask install google-cloud-sdk
brew cask install google-drive
brew cask install google-japanese-ime
brew cask install grandperspective
brew cask install gyazo
brew cask install handbrake
brew cask install istat-menus
brew cask install iterm2
brew cask install karabiner
brew cask install kobito
brew cask install launchbar
brew cask install licecap
brew cask install limechat
brew cask install lyn
brew cask install quicksilver
brew cask install ripit
brew cask install sequel-pro
brew cask install silverlight
brew cask install skype
brew cask install sleipnir
brew cask install sourcetree
brew cask install sublime-text
brew cask install trailer
brew cask install vagrant
brew cask install virtualbox
brew cask install vlc
