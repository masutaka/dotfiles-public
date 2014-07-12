# masutaka's original .zshenv for zsh 4.0.2-later

OS_KIND=`uname`

#---------------------------------------------------------------------
# Environment variables
#---------------------------------------------------------------------
case "$OS_KIND" in
Linux)
	if [ -d "$HOME/.plenv" ]; then
		export PATH=$HOME/.plenv/bin:$PATH
		eval "$(plenv init -)"
	fi
	if [ -d "$HOME/.rbenv" ]; then
		export PATH=$HOME/.rbenv/bin:$PATH
		eval "$(rbenv init -)"
	fi
	export EDITOR=emacsclient
	export LDFLAGS="-s"
	;;
Darwin)
	export GOPATH=$HOME
	export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin
	export PATH=$HOME/opt/bin:$GOPATH/bin:/Applications/Emacs.app/Contents/MacOS/bin:$PATH:/usr/local/mysql/bin:$HOME/.phpenv/bin
	export MANPATH=$HOME/.emacs.d/share/man:/usr/local/mysql/man:$MANPATH
	export EDITOR=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
	export NVM_DIR=$HOME/.nvm
	source $(brew --prefix nvm)/nvm.sh
	eval "$(phpenv init -)"
	eval "$(plenv init -)"
	eval "$(rbenv init -)"
	eval "$(direnv hook zsh)"
	;;
*)
	echo "Unkonwn OS" > /dev/stderr
	;;
esac

# New File => 644, New Dir => 755
umask 022

# No core files by default
ulimit -S -c 0 > /dev/null 2>&1

export MYSQL_PS1="(\u@`hostname`) [\d] > "

export DOCKER_HOST=tcp://localhost:4243

# Local Variables:
# coding: utf-8
# mode: shell-script
# tab-width: 4
# End: