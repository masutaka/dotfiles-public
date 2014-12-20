# masutaka's original .zshenv for zsh 4.0.2-later

OS_KIND=`uname`

#---------------------------------------------------------------------
# Environment variables
#---------------------------------------------------------------------
case "$OS_KIND" in
Darwin)
	export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin
	export PATH=$HOME/opt/bin:$HOME/.cask/bin:$HOME/opt/terraform:$PATH:/usr/local/mysql/bin
	export MANPATH=$HOME/.emacs.d/share/man:/usr/local/mysql/man:$MANPATH
	export EDITOR=$HOME/opt/emacs-24.4/bin/emacsclient
	export NVM_DIR=$HOME/.nvm
	source /usr/local/opt/nvm/nvm.sh
    source /opt/homebrew-cask/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc
#   source /opt/homebrew-cask/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc
	;;
Linux)
	export LDFLAGS="-s"
	;;
*)
	echo "Unkonwn OS" > /dev/stderr
	;;
esac

export GOPATH=$HOME
export PATH=$HOME/.plenv/bin:$HOME/.rbenv/bin:$GOPATH/bin:$PATH
eval "$(plenv init -)"
eval "$(rbenv init -)"
eval "$(direnv hook zsh)"

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
