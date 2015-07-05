#!/bin/sh

echo "brew updating..."

brew update
outdated=`brew outdated`

if [ -n "$outdated" ]; then
	cat <<EOF

The following package(s) will upgrade.

$outdated

Are you sure?
If you don't want to upgrade, please type Ctrl-c now.
EOF

	read dummy

	brew upgrade --all
fi

if [ $(hostname) = "masutaka-pc.local" ]; then
	PRIVATE_MACHINE=1
fi

brew install ack
brew install ansible
brew install aspell
brew install autoconf # emacs with inline.patch needs autoheader
brew install binutils
brew install boot2docker
brew install cmake # for gem octodown
brew install cmigemo
brew install docker
brew install git
brew install gnu-sed
brew install gnupg
brew install go
brew install heroku-toolbelt
brew install homebrew/binary/jsl
brew install homebrew/binary/kindlegen
brew install icu4c # for gem octodown
brew install imagemagick
brew install jq
brew install lv
brew install markdown
brew install mercurial
brew install pkg-config # for gem ref
brew install postgresql
brew install qt # gem capybara-screenshot uses qmake
brew install readline # for Ruby
brew install redis
brew install terminal-notifier
brew install tree
brew install unrar
brew install webkit2png # $ webkit2png -TF http://masutaka.net
brew install wget
brew install xz

if [ -n "$PRIVATE_MACHINE" ]; then
	brew install nginx
	brew install youtube-dl
fi

cat <<EOF
Do you want to install brew casks?
If you don't want to install, please type Ctrl-c now.
EOF

read dummy

brew install caskroom/cask/brew-cask
brew cask install appcleaner
brew cask install atom
brew cask install caffeine
brew cask install dropbox
brew cask install firefox --caskroom=/Applications
brew cask install flash
brew cask install google-chrome --caskroom=/Applications
brew cask install google-cloud-sdk
brew cask install google-drive
brew cask install google-japanese-ime
brew cask install grandperspective
brew cask install gyazo
brew cask install imageoptim
brew cask install karabiner
brew cask install launchbar
brew cask install licecap
brew cask install mysqlworkbench
brew cask install quicksilver
brew cask install silverlight
brew cask install trailer
brew cask install vagrant
brew cask install virtualbox

if [ -n "$PRIVATE_MACHINE" ]; then
	brew cask install chromecast
	brew cask install flip4mac
	brew cask install handbrake
	brew cask install lyn
	brew cask install ripit
	brew cask install skype
	brew cask install sleipnir
	brew cask install vlc
fi
