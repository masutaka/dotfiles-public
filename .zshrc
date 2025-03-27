#---------------------------------------------------------------------
# Functions
#---------------------------------------------------------------------

function backup-elpa () {
  local f=elpa-$(date '+%Y%m%d%H%M%S').tar.gz
  (cd $HOME/.emacs.d && tar czf "$f" elpa && trash "$f")
}

function exists () {
  type $1 > /dev/null
}

# Usage: $ fingerprints <".pub" or authorized_keys>
# https://gist.github.com/hvr/662196
function fingerprints {
  while read key; do
    if [[ "$key" =~ "^#|^$" ]]; then
      continue
    fi
    printf "%-16s => " $(echo "$key" | cut -f 3 -d ' ')
    echo "$key" | ssh-keygen -l -E md5 -f -
  done < $1
}

function go-installs () {
  for i in $(cat $HOME/src/github.com/masutaka/dotfiles/go.txt); do
    echo $i
    go install $i
  done
}
alias go-updates=go-installs

function psme () {
  ps auxw$1 | egrep "^(USER|$USER)" | sort -k 2 -n
}

function psnot () {
  ps auxw$1 | egrep -v "^$USER" | sort -k 2 -n
}

function rm-local-branches () {
  local base_branch=main
  if [ -n "$MY_BASE_BRANCH" ]; then
    base_branch=$MY_BASE_BRANCH
  elif git rev-parse --verify master > /dev/null 2>&1; then
    base_branch=master
  fi

  echo "base_branch: $base_branch"

  local git="echo git"
  if [ "$1" = "-f" ]; then
    git="git"
  fi

  for b in $(git branch --merged "origin/$base_branch" | grep -Fvw "$base_branch" | awk '{print $1}'); do
    eval "$git branch -d $b"
  done
}

if [ "$OS_KIND" = Darwin ]; then
  function unixtime2date () {
    date -r $1 +%Y-%m-%dT%H:%M:%S%z
  }

  function kd () {
    ls -alF $@ | more -e
  }
else
  function unixtime2date () {
    date --date="@$1" +%Y-%m-%dT%H:%M:%S%z
  }

  function kd () {
    LC_COLLATE=C ls -alF $@ | more -e
  }
fi

#---------------------------------------------------------------------
# Shell variables
#---------------------------------------------------------------------

# プロンプト(man zshmisc)
if [ "$OS_KIND" = Darwin ]; then
  PROMPT='%B%U%m%u:%~ $%b '
else
  PROMPT='%B%U%M%u:%~ $%b '
fi

FPATH=$HOME/.docker/completions:$FPATH

# 履歴を保存するファイル
HISTFILE=$HOME/.zhistory

# メモリ内の履歴の数
HISTSIZE=1000000

# $HISTFILE に保存される履歴の数
SAVEHIST=1000000

# 新規メールが来ても、メッセージを出さない。
#MAILCHECK=0

#---------------------------------------------------------------------
# Shell options
#---------------------------------------------------------------------
# tab キーを押したときに beep しない。
setopt nolistbeep

# cd の際に自動的に pushdしてくれる。(for alias=d)
setopt auto_pushd

# コマンドの問い合わせ訂正
setopt correct

# {a-c} を a b c に展開する機能が使える。
#setopt brace_ccl

# ワイルドカード拡張、ファイル名で #, ~, ^ の 3 文字を正規表現として扱う。(man zshexpn)
setopt extended_glob

# $HISTFILEに時間も記録
setopt extended_history

# 同じコマンドの history は履歴に入れない
setopt hist_ignore_all_dups

# 前回と同じコマンドの history は履歴に入れない
setopt hist_ignore_dups

# コマンドラインの先頭にスペースをいれておくとそのコマンドはヒストリに追加しない。
setopt hist_ignore_space

# バックグラウンド・ジョブの終了を即座に通知する。
setopt notify

# コマンドの返り値が 0 以外の時に表示してくれる。
#setopt print_exit_value

# プロンプトに環境変数やエスケープシーケンスを含める。
setopt prompt_subst

# 既にpushdしたディレクトリはダブらせずにディレクトリスタックの先頭に持って来る。
setopt pushd_ignore_dups

# 他の端末と履歴の同期を取る。
# この設定が合わなければ、適当なタイミングで fc -RI しても良いかも。
setopt share_history

# 既存のファイルへの上書きリダイレクト防止
unsetopt clobber

# シェル終了時に子プロセスに HUP を送らない
setopt nocheckjobs nohup

# ディレクトリ名だけでcd
setopt auto_cd

# Ctrl-Sで端末を固まらせない。
stty -ixon

# tetris
#autoload -Uz tetris; zle -N tetris

## まともな kill の補完にする。
zstyle ':completion:*:processes' command 'ps x -o pid,s,args'

## 補完時に大文字小文字を区別しない。
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# 補完候補を C-fとかC-nとかで選択できる。
zstyle ':completion:*:default' menu select=1

# $FPATH 以下にある補完コレクションを使う。
autoload -Uz compinit && compinit -u

# for hook
autoload -Uz add-zsh-hook

# Command Completion for AWS CLI
if [ "$OS_KIND" = Linux ] && exists aws; then
  source ${PYTHONUSERBASE}/bin/aws_zsh_completer.sh
fi

#---------------------------------------------------------------------
# cdr
#---------------------------------------------------------------------

autoload -Uz chpwd_recent_dirs cdr
add-zsh-hook chpwd chpwd_recent_dirs
mkdir -p "${XDG_CACHE_HOME}/shell"
zstyle ':completion:*:*:cdr:*:*' menu selection
zstyle ':completion:*' recent-dirs-insert both
zstyle ':chpwd:*' recent-dirs-max 500
zstyle ':chpwd:*' recent-dirs-default true
zstyle ':chpwd:*' recent-dirs-file "${XDG_CACHE_HOME}/shell/chpwd-recent-dirs"
zstyle ':chpwd:*' recent-dirs-pushd true

#---------------------------------------------------------------------
# Homebrew
#---------------------------------------------------------------------

if [ "$OS_KIND" = Darwin ]; then
  # Don't automatically cleanup on reinstall, install or upgrade
  export HOMEBREW_NO_INSTALL_CLEANUP=yes

  function my-brew-upgrade () {
    echo "brew updating..."

    brew update
    outdated=$(brew outdated)

    if [ -n "$outdated" ]; then
      cat <<EOF

The following package(s) will upgrade.

$outdated

Are you sure?
If you don't want to upgrade, please type Ctrl-c now.
EOF

      read dummy

      brew cleanup
      brew upgrade
    fi

    brew doctor
  }
fi

#---------------------------------------------------------------------
# RPROMPT
#---------------------------------------------------------------------

function aws_prompt () {
  if exists aws; then
    local profile=${AWS_PROFILE:=default}
    echo "%F{214}(AWS:${profile})%f"
  fi
}

function google_cloud_prompt () {
  if exists gcloud; then
    local profile=${CLOUDSDK_ACTIVE_CONFIG_NAME:=default}
    echo "%F{039}(GC:${profile})%f"
  fi
}

# show vcs branch name to $RPROMPT

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn hg bzr
zstyle ':vcs_info:*' formats '(%s:%b)'
zstyle ':vcs_info:*' actionformats '(%s:%b|%a)'
zstyle ':vcs_info:(svn|bzr):*' branchformat '%b:r%r'
zstyle ':vcs_info:bzr:*' use-simple true

function vcs_info_precmd () {
  psvar=()
  LANG=en_US.UTF-8 vcs_info
  [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}
add-zsh-hook precmd vcs_info_precmd

function vcs_prompt () {
  echo '%1(v|%F{green}%1v%f|)'
}

# See also "$ man zshmisc"
RPROMPT='[%*]$(aws_prompt)$(google_cloud_prompt)$(vcs_prompt)'

#---------------------------------------------------------------------
# peco
#---------------------------------------------------------------------

if exists peco; then
  function peco_select_history () {
    local tac
    exists gtac && tac="gtac" || { exists tac && tac="tac" || { tac="tail -r" } }
    BUFFER=$(fc -l -n 1 | eval $tac | peco --query "$LBUFFER")
    CURSOR=$#BUFFER         # move cursor
  }
  zle -N peco_select_history
  bindkey '^R' peco_select_history

  function peco_bundle_show () {
    local selected_dir=$(bundle show | awk 'NR>1 {print $2}' | peco | xargs bundle show)
    if [ -n "$selected_dir" ]; then
      BUFFER="cd ${selected_dir}"
      zle accept-line
    fi
  }
  zle -N peco_bundle_show
  bindkey '^xy' peco_bundle_show

  function peco_helm () {
    local IFS="
"
    my-compact-chpwd-recent-dirs
    local selected_dir=$((ghq list --full-path | sed -e "s@$HOME@~@";
                          cdr -l | perl -pne 's@^[0-9]+ +@@') | awk '!x[$0]++{print $0}' | peco)
    if [ -n "$selected_dir" ]; then
      BUFFER="cd ${selected_dir}"
      zle accept-line
    fi
  }
  zle -N peco_helm
  bindkey '^x^b' peco_helm

  # http://blog.n-z.jp/blog/2014-07-25-compact-chpwd-recent-dirs.html
  function my-compact-chpwd-recent-dirs () {
    emulate -L zsh
    setopt extendedglob
    local -aU reply
    integer history_size
    autoload -Uz chpwd_recent_filehandler
    chpwd_recent_filehandler
    history_size=$#reply
    reply=(${^reply}(N))
    (( $history_size == $#reply )) || chpwd_recent_filehandler $reply
  }

  function peco-pkill () {
    for pid in $(ps aux | peco | awk '{ print $2 }'); do
      kill $pid
      echo "Killed ${pid}"
    done
  }
  alias pk="peco-pkill"

  function peco-gcloud-configurations () {
    local selected_branch=$(gcloud config configurations list | peco | grep -v '^NAME ' | cut -d ' ' -f1)
    if [ -n "$selected_branch" ]; then
      BUFFER="export CLOUDSDK_ACTIVE_CONFIG_NAME=${selected_branch}"
      zle accept-line
    fi
  }
  zle -N peco-gcloud-configurations
  bindkey '^x^k' peco-gcloud-configurations

  function peco-git-recent-branches () {
    local selected_branch=$(git branch --sort=-authordate -v | peco | sed -E -e 's/^[* ]+//' | cut -d ' ' -f1)
    if [ -n "$selected_branch" ]; then
      BUFFER="git checkout ${selected_branch}"
      zle accept-line
    fi
  }
  zle -N peco-git-recent-branches
  bindkey '^xn' peco-git-recent-branches

  function peco-git-recent-all-branches () {
    local selected_branch=$(git branch --sort=-authordate -v -a | peco | sed -E -e 's/^[* ]+//' | cut -d ' ' -f1)
    if [ -n "$selected_branch" ]; then
      BUFFER="git checkout -t ${selected_branch}"
      zle accept-line
    fi
  }
  zle -N peco-git-recent-all-branches
  bindkey '^x^n' peco-git-recent-all-branches
fi

#---------------------------------------------------------------------
# Key binding
#---------------------------------------------------------------------

function my-backward-kill-word () {
  local WORDCHARS="${WORDCHARS:s#/#}"
  zle backward-kill-word
}
zle -N my-backward-kill-word
bindkey '^[h' my-backward-kill-word

function my-backward-word () {
  local WORDCHARS="${WORDCHARS:s#/#}"
  zle backward-word
}
zle -N my-backward-word
bindkey '^[b' my-backward-word # 本当は C-, を使いたい。

function my-forward-word () {
  local WORDCHARS="${WORDCHARS:s#/#}"
  zle forward-word
}
zle -N my-forward-word
bindkey '^[f' my-forward-word  # 本当は C-. を使いたい。

# C-x C-p で直前の履歴をクリップボードにコピー
if [ "$OS_KIND" = "Darwin" ]; then
  COPY2CLIPBOARD="pbcopy"
else
  COPY2CLIPBOARD="xsel -b"
fi
function copy-last-history () {
  zle up-line-or-history
  print -rn "\$ $BUFFER" | eval "$COPY2CLIPBOARD"
  zle kill-whole-line
}
zle -N copy-last-history
bindkey '^x^p' copy-last-history

# ファイル名で補完させる。
function _du () { _files }

# shell-mode風
autoload -Uz history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^[p" history-beginning-search-backward-end
bindkey "^[n" history-beginning-search-forward-end

# glob(*) で履歴をインクリメンタル検索可能。
#bindkey '^R' history-incremental-pattern-search-backward
bindkey '^S' history-incremental-pattern-search-forward

bindkey '^[?'  run-help      # カーソル下の manを表示。
bindkey '^q^q' quoted-insert
bindkey '^v'   undefined-key
bindkey '^w'   kill-region

#---------------------------------------------------------------------
# Aliases
#---------------------------------------------------------------------
if [ "$OS_KIND" = Linux ]; then
  alias emacs="LC_COLLATE=C emacs"
  alias pbcopy="xsel -b"
fi

if exists peco; then
  alias -g B='$(git branch | peco | sed -e "s/^\*[ ]*//")'
fi

alias -g G='2>&1 | grep'
alias -g L='2>&1 | less'

alias cp="cp -i"
alias mv="mv -i"
alias rm="rm -i"

alias cdg="cd \$(git rev-parse --show-toplevel)"
alias d=docker
alias g=git
alias hall="history -E -i 1"

# Local Variables:
# tab-width: 8
# End:
