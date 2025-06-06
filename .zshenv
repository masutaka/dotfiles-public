OS_KIND=$(uname)

#---------------------------------------------------------------------
# Environment variables
#---------------------------------------------------------------------
export XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
export XDG_DATA_HOME=${XDG_DATA_HOME:-$HOME/.local/share}
export XDG_STATE_HOME=${XDG_STATE_HOME:-$HOME/.local/state}

case "$OS_KIND" in
Darwin)
  PATH=/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin

  # Set PATH, MANPATH, etc., for Homebrew.
  eval "$(/opt/homebrew/bin/brew shellenv)"

  if [ -d "${HOMEBREW_PREFIX}/opt/libpq/bin" ]; then
    PATH=${HOMEBREW_PREFIX}/opt/libpq/bin:$PATH
  fi

  if [ -d "${HOME}/.local/bin" ]; then
    PATH=$HOME/.local/bin:$PATH
  fi

  if [ -d "${HOME}/.tfenv/bin" ]; then
    PATH=${HOME}/.tfenv/bin:$PATH
  fi

  if [ -d "${HOMEBREW_PREFIX}/share/google-cloud-sdk" ]; then
    source ${HOMEBREW_PREFIX}/share/google-cloud-sdk/path.zsh.inc
    source ${HOMEBREW_PREFIX}/share/google-cloud-sdk/completion.zsh.inc
  fi
  ;;
Linux)
  source /opt/google-cloud-cli/path.zsh.inc
  source /opt/google-cloud-cli/completion.zsh.inc
  ;;
*)
  echo "Unkonwn OS" > /dev/stderr
  ;;
esac

# GOPATH を設定しつつ、直下の bin をパスに通す。
export GOPATH=$HOME/go:$HOME
PATH=$(echo $GOPATH | sed -e 's@:@/bin:@g' -e 's@$@/bin@'):$PATH

# pip に --user オプションを付けた時のインストール先を変えつつ、直下の bin をパスに通す。
export PYTHONUSERBASE="${HOME}/python"
PATH=${PYTHONUSERBASE}/bin:$PATH

# Rust
if [ -r "$HOME/.cargo/env" ]; then
  source "$HOME/.cargo/env"
fi

export ACKRC="${XDG_CONFIG_HOME}/ack/ackrc"
export BIGQUERYRC="${XDG_CONFIG_HOME}/bq/bigqueryrc"
export EDITOR=emacsclient
export GEMSRC_USE_GHQ=1
export IRBRC="${XDG_CONFIG_HOME}/ruby/irbrc"
export LANG=en_US.UTF-8 # Avoid ack warning
export MYSQL_PS1="(\u@$(hostname)) [\d] > "
export NPM_CONFIG_USERCONFIG="${XDG_CONFIG_HOME}/npm/config"
export TF_PLUGIN_CACHE_DIR="${XDG_CACHE_HOME}/terraform/plugin-cache"

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

# 重複したパスを取り除く
typeset -U PATH

# New File => 644, New Dir => 755
umask 022

# No core files by default
ulimit -S -c 0

# /etc/zprofile 等の /etc 以下のファイルを読み込ませない
setopt no_global_rcs

if type direnv > /dev/null; then
  eval "$(direnv hook zsh)"
fi

#---------------------------------------------------------------------
# asdf
#---------------------------------------------------------------------

MY_ASDF_CONFIG_HOME="${XDG_CONFIG_HOME}/asdf"
export ASDF_CONFIG_FILE="${MY_ASDF_CONFIG_HOME}/asdfrc"
export ASDF_DATA_DIR="${XDG_DATA_HOME}/asdf"

export ASDF_GEM_DEFAULT_PACKAGES_FILE="${MY_ASDF_CONFIG_HOME}/default-gems"
export ASDF_NPM_DEFAULT_PACKAGES_FILE="${MY_ASDF_CONFIG_HOME}/default-npm-packages"
export ASDF_PERL_DEFAULT_PACKAGES_FILE="${MY_ASDF_CONFIG_HOME}/default-perl-modules"
export ASDF_PYTHON_DEFAULT_PACKAGES_FILE="${MY_ASDF_CONFIG_HOME}/default-python-packages"

PATH=$ASDF_DATA_DIR/shims:$PATH

# Local Variables:
# tab-width: 8
# End:
