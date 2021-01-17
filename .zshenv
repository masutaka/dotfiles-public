# masutaka's original .zshenv for zsh 5.0.5 later

OS_KIND=$(uname)

#---------------------------------------------------------------------
# Environment variables
#---------------------------------------------------------------------
case "$OS_KIND" in
Darwin)
  export PATH=/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin
  export EDITOR=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
  export EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
  source /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc
  source /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc

  if [ -d /usr/local/opt/postgresql@10/bin ]; then
	PATH=/usr/local/opt/postgresql@10/bin:$PATH
  fi
  ;;
Linux)
  ;;
*)
  echo "Unkonwn OS" > /dev/stderr
  ;;
esac

export GOPATH=$HOME/go:$HOME
export GEMSRC_USE_GHQ=1
export PATH=$(echo $GOPATH | sed -e 's@:@/bin:@g' -e 's@$@/bin@'):$PATH

if type direnv > /dev/null; then
  eval "$(direnv hook zsh)"
fi

if [ -d /usr/local/opt/zplug ]; then
  export ZPLUG_HOME=/usr/local/opt/zplug
  source $ZPLUG_HOME/init.zsh
fi

# New File => 644, New Dir => 755
umask 022

# No core files by default
ulimit -S -c 0

export MYSQL_PS1="(\u@$(hostname)) [\d] > "

# Avoid ack warning
export LANG=en_US.UTF-8

# -g   検索したとき、ヒットした全ての文字列を反転するのではなく、現在カーソルがある行のみ反転する
# -i   検索時に全部小文字で入力したときだけ、大文字小文字を無視する
# -M   -m より冗長なプロンプトを使う
# -R   ANSI カラーエスケープシーケンスを解するようになる
# -S   一行が長く、ターミナルの幅が狭くて表示できない場合、途中までしか表示しない
# -W   一度に 2 行以上移動した場合、新たに表示した最初の行をハイライトする
# -X   終了時に画面クリアしない
# -z-4 ページスクロール時、画面の行数-4行だけスクロール
# -x4  tab-stop を 4 とする
export LESS="-g -i -M -R -S -W -X -z-4 -x4"

export WHALEBREW_INSTALL_PATH=$HOME/bin

# Don't automatically cleanup on reinstall, install or upgrade
export HOMEBREW_NO_INSTALL_CLEANUP=yes

# /etc/zprofile 等の /etc 以下のファイルを読み込ませない
setopt no_global_rcs

# Local Variables:
# coding: utf-8
# mode: shell-script
# tab-width: 4
# End:
