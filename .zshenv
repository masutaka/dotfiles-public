# masutaka's original .zshenv for zsh 5.0.5 later

OS_KIND=$(uname)

#---------------------------------------------------------------------
# Environment variables
#---------------------------------------------------------------------
case "$OS_KIND" in
Darwin)
  export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin
  export EDITOR=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
  export EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
  source /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc
  source /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc
  ;;
Linux)
  export LDFLAGS="-s"
  ;;
*)
  echo "Unkonwn OS" > /dev/stderr
  ;;
esac

export GOPATH=$HOME/go:$HOME
export GEMSRC_USE_GHQ=1
export PATH=$HOME/.nodenv/bin:$HOME/.plenv/bin:$HOME/.rbenv/bin:$HOME/.cargo/bin:$PATH
eval "$(nodenv init -)"
eval "$(plenv init --no-rehash -)"
eval "$(rbenv init --no-rehash -)"
export PATH=$(echo $GOPATH | sed -e 's@:@/bin:@g' -e 's@$@/bin@'):$PATH

if type direnv > /dev/null; then
  eval "$(direnv hook zsh)"
fi

if [ "$OS_KIND" = "Darwin" ]; then
  export ZPLUG_HOME=/usr/local/opt/zplug
  source $ZPLUG_HOME/init.zsh
fi

# New File => 644, New Dir => 755
umask 022

# No core files by default
ulimit -S -c 0

export AWS_REGION=ap-northeast-1

export MYSQL_PS1="(\u@$(hostname)) [\d] > "

# Avoid ack warning
export LANG=en_US.UTF-8

export LESS="-g -i -M -R -S -W -z-4 -x4"

export WHALEBREW_INSTALL_PATH=$HOME/bin

# /etc/zprofile 等の /etc 以下のファイルを読み込ませない
setopt no_global_rcs

# Local Variables:
# coding: utf-8
# mode: shell-script
# tab-width: 4
# End:
