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
	export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin
	export PATH=$HOME/bin:/Applications/Emacs.app/Contents/MacOS/bin:$PATH:/usr/local/mysql/bin:$HOME/.phpenv/bin
	export MANPATH=$HOME/.emacs.d/share/man:/usr/local/mysql/man:$MANPATH
	export EDITOR=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
	eval "$(phpenv init -)"
	eval "$(plenv init -)"
	eval "$(rbenv init -)"
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

#---------------------------------------------------------------------
# Functions
#---------------------------------------------------------------------
if [ "$EMACS" = "t" ]; then
	function kd() {
		ls -alF $*
	}
else
	function kd() {
		ls -alF $* | more
	}
fi

function psme() {
	ps auxw$1 | egrep "^(USER|$USER)" | sort -k 2 -n
}

function psnot() {
	ps auxw$1 | egrep -v "^$USER" | sort -k 2 -n
}

#---------------------------------------------------------------------
# Aliases
#---------------------------------------------------------------------
if [ "$OS_KIND" = Darwin ]; then
	alias emacs=/Applications/Emacs.app/Contents/MacOS/Emacs
	alias emacsdevel=/Applications/Emacs-devel.app/Contents/MacOS/Emacs
fi

# eval "$(hub alias -s)"
if alias git > /dev/null; then
	unalias git
fi

if [ "$EMACS" = "t" ]; then
	alias git="git --no-pager"
fi

alias be="bundle exec"
alias pe="plenv exec"

alias d='dirs -v; echo -n "select number: "; read newdir; cd +"$newdir"'

alias cp="cp -i"
alias mv="mv -i"
alias rm="rm -i"

alias cdg="cd \$(git rev-parse --show-toplevel)"
alias e="eval $EDITOR"
alias expandurl="perl -MLWP::UserAgent -lE 'say LWP::UserAgent->new->head(shift)->request->uri'"
alias g="git"
alias hall="history -E -i 1"
alias rootinstalllog="echo 'find /usr/local -cnewer timestamp | sort'"
alias v="vagrant"

# Local Variables:
# coding: utf-8
# mode: shell-script
# tab-width: 4
# End:
