# masutaka's original .zshenv for zsh 5.0.5 later

OS_KIND=`uname`

#---------------------------------------------------------------------
# Environment variables
#---------------------------------------------------------------------
case "$OS_KIND" in
Darwin)
	export PATH=$HOME/opt/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin
	export EDITOR=emacsclient
	export EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
    source /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc
#   source /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc
	;;
Linux)
	export LDFLAGS="-s"
	;;
*)
	echo "Unkonwn OS" > /dev/stderr
	;;
esac

export GOPATH=$HOME
export PATH=$HOME/.nodebrew/current/bin:$HOME/.plenv/bin:$HOME/.rbenv/bin:$GOPATH/bin:$PATH
export PATH=$(ghg bin):$PATH
export NODE_PATH=$(npm root -g 2> /dev/null)
eval "$(plenv init -)"
eval "$(rbenv init -)"
eval "$(direnv hook zsh)"

# New File => 644, New Dir => 755
umask 022

# No core files by default
ulimit -S -c 0 > /dev/null 2>&1

export MYSQL_PS1="(\u@`hostname`) [\d] > "

#eval "$(docker-machine env default)"

# Local Variables:
# coding: utf-8
# mode: shell-script
# tab-width: 4
# End:
