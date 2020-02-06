# masutaka's original .zshrc for zsh 5.0.5 later

#---------------------------------------------------------------------
# Functions
#---------------------------------------------------------------------

function brew-all-deps() {
  for formula in $(brew list); do
	echo -n $fg[blue] $formula $fg[white]
	brew deps $formula | awk '{printf(" %s", $0)}'
	echo
  done
}

function exists() {
  type $1 > /dev/null
}

function go-update() {
  for i in $(cat $HOME/src/github.com/masutaka/dotfiles/anyenvs/go.txt); do
	echo $i
	go get -u $i
  done
}

if [ "$OS_KIND" = Darwin ]; then
  function kd() {
	ls -alF $* | more -e
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

function svndiff() {
  svn diff $* | vim -R -
}

if [ "$OS_KIND" = Darwin ]; then
  function epoch2date() {
	date -r $1 +%Y-%m-%dT%H:%M:%S%z
  }
else
  function epoch2date() {
	date --date="@$1" +%Y-%m-%dT%H:%M:%S%z
  }
fi

#---------------------------------------------------------------------
# Shell variables
#---------------------------------------------------------------------

function aws_prompt() {
  local profile=default

  if [ -n "$AWS_PROFILE" ]; then
	profile=$AWS_PROFILE
  fi

  echo "%F{yellow}(aws:${profile})%f"
}

# プロンプト(man zshmisc)
if [ "$OS_KIND" = Darwin ]; then
  PROMPT='%B%U%m%u:%~ $%b '
else
  PROMPT='%B%U%M%u:%~ $%b '
fi
RPROMPT='[%*]$(aws_prompt)%1(v|%F{green}%1v%f|)'

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
#setopt	brace_ccl

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
source /usr/local/share/zsh/site-functions/aws_zsh_completer.sh

#---------------------------------------------------------------------
# asdf
#---------------------------------------------------------------------

source $HOME/.asdf/asdf.sh
source $HOME/.asdf/completions/asdf.bash

export ASDF_CONFIG_FILE="${XDG_CONFIG_HOME:-$HOME/.config}/asdf/asdfrc"
export ASDF_DEFAULT_TOOL_VERSIONS_FILENAME="${XDG_CONFIG_HOME:-$HOME/.config}/asdf/tool-versions"
export ASDF_RUBY_BUILD_VERSION=master

#---------------------------------------------------------------------
# cdr
#---------------------------------------------------------------------

autoload -Uz chpwd_recent_dirs cdr
add-zsh-hook chpwd chpwd_recent_dirs
mkdir -p "${XDG_CACHE_HOME:-$HOME/.cache}/shell"
zstyle ':completion:*:*:cdr:*:*' menu selection
zstyle ':completion:*' recent-dirs-insert both
zstyle ':chpwd:*' recent-dirs-max 500
zstyle ':chpwd:*' recent-dirs-default true
zstyle ':chpwd:*' recent-dirs-file "${XDG_CACHE_HOME:-$HOME/.cache}/shell/chpwd-recent-dirs"
zstyle ':chpwd:*' recent-dirs-pushd true

#---------------------------------------------------------------------
# show vcs branch name to $RPROMPT
#---------------------------------------------------------------------

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn hg bzr
zstyle ':vcs_info:*' formats '(%s:%b)'
zstyle ':vcs_info:*' actionformats '(%s:%b|%a)'
zstyle ':vcs_info:(svn|bzr):*' branchformat '%b:r%r'
zstyle ':vcs_info:bzr:*' use-simple true

function vcs_info_precmd() {
  psvar=()
  LANG=en_US.UTF-8 vcs_info
  [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}
add-zsh-hook precmd vcs_info_precmd

#---------------------------------------------------------------------
# zplug
#---------------------------------------------------------------------

# 後述の bindkey '^xn' peco-git-recent-branches が _next_tags で
# 置き換えられてしまうので、ここに置く。

if exists zplug; then
  zplug 'bfirsh/whalebrew', from:gh-r, as:command, use:'*Darwin*x86_64*'
  zplug 'wantedly/dockertags', from:gh-r, as:command, use:'*darwin*amd64*'
  zplug 'wata727/tflint', from:gh-r, as:command, use:'*darwin*amd64*'

  if ! zplug check; then
	zplug install
  fi

  zplug load
fi

#---------------------------------------------------------------------
# Install Manually
#---------------------------------------------------------------------

if ! exists trs; then
   curl https://raw.githubusercontent.com/kogai/trs/master/install.sh -Ssf | sh
fi

#---------------------------------------------------------------------
# peco
#---------------------------------------------------------------------

if exists peco; then
  function peco_select_history() {
	local tac
	exists gtac && tac="gtac" || { exists tac && tac="tac" || { tac="tail -r" } }
	BUFFER=$(fc -l -n 1 | eval $tac | peco --query "$LBUFFER")
	CURSOR=$#BUFFER         # move cursor
  }
  zle -N peco_select_history
  bindkey '^R' peco_select_history

  function peco_bundle_show() {
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

  function peco-pkill() {
	for pid in $(ps aux | peco | awk '{ print $2 }'); do
	  kill $pid
	  echo "Killed ${pid}"
	done
  }
  alias pk="peco-pkill"

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
# screen mode-line
#---------------------------------------------------------------------

if [ "$TERM" = "screen" ]; then
  # コマンド実行中はコマンド名を、未実行ならカレントディレクトリを表示する。

  function screen_mode_line_preexec() {
	echo -ne "\ek#${1%% *}\e\\"
  }
  add-zsh-hook preexec screen_mode_line_preexec

  function screen_mode_line_precmd() {
	echo -ne "\ek$(basename $(pwd))\e\\"
  }
  add-zsh-hook precmd screen_mode_line_precmd
fi

#---------------------------------------------------------------------
# tfschema
#---------------------------------------------------------------------

# zplug の設定より後ろにする必要がある

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /usr/local/bin/tfschema tfschema

#---------------------------------------------------------------------
# Function
#---------------------------------------------------------------------

# Usage: $ fingerprints <filename>
# https://gist.github.com/hvr/662196
function fingerprints {
  IGNORE_LINES="^#|^$"

  (
	while read line; do
	  echo $line >! /tmp/pubkey
      if [[ "$line" =~ $IGNORE_LINES ]]; then
        continue
      fi
      printf "%-16s => " $(cut -f 3 -d ' ' /tmp/pubkey)

	  # Changed default fingerprint hash from OpenSSH 6.8/6.8p1
	  if [ "$OS_KIND" = "Darwin" ]; then
		ssh-keygen -l -E md5 -f /tmp/pubkey
	  else
		ssh-keygen -l -f /tmp/pubkey
	  fi
	done
	rm -f /tmp/pubkey
  ) < $1
}

function urlencode() {
  echo $(php -r "echo rawurlencode('$1');")
}

function urldecode() {
  echo $(php -r "echo rawurldecode('$1');")
}

function userstack() {
  access_key_file="${XDG_CONFIG_HOME:-$HOME/.config}/userstack/access_key"

  if [ ! -f "$access_key_file" ]; then
	echo "$access_key_file not found."
	return 1
  fi

  legacy=$2
  if [ -z "$legacy" ]; then
	 legacy=0 # 0 or 1
  fi

  curl -s "http://api.userstack.com/detect?access_key=$(cat $access_key_file)&ua=$(urlencode $1)&legacy=$legacy" | jq .
}

if [ "$OS_KIND" = "Darwin" ]; then
  # http://qiita.com/kyanny/items/0797d37cab6327fba2c4
  function ciopen() {
	commit=head
	if [ -n "$1" ]; then
	  commit=$1
	fi

	result=$(hub ci-status -v $commit)

	if [ $? = 0 ]; then
	  open $(echo $result | awk '{print $NF}')
	fi
  }
fi

#---------------------------------------------------------------------
# Key binding
#---------------------------------------------------------------------

function my-backward-kill-word() {
  local WORDCHARS="${WORDCHARS:s#/#}"
  zle backward-kill-word
}
zle -N my-backward-kill-word
bindkey '^[h' my-backward-kill-word

function my-backward-word() {
  local WORDCHARS="${WORDCHARS:s#/#}"
  zle backward-word
}
zle -N my-backward-word
bindkey '^[b' my-backward-word	# 本当は C-, を使いたい。

function my-forward-word() {
  local WORDCHARS="${WORDCHARS:s#/#}"
  zle forward-word
}
zle -N my-forward-word
bindkey '^[f' my-forward-word	# 本当は C-. を使いたい。

if [ "$OS_KIND" = "Darwin" ]; then
  # C-x C-p で直前の履歴をクリップボードにコピー
  pbcopy-last-history(){
	zle up-line-or-history
	print -rn "\$ $BUFFER" | pbcopy
	zle kill-whole-line
  }
  zle -N pbcopy-last-history
  bindkey '^x^p' pbcopy-last-history
fi

# ファイル名で補完させる。
function _du() { _files }

# shell-mode風
autoload -Uz history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^[p" history-beginning-search-backward-end
bindkey "^[n" history-beginning-search-forward-end

# glob(*) で履歴をインクリメンタル検索可能。
#bindkey '^R' history-incremental-pattern-search-backward
bindkey '^S' history-incremental-pattern-search-forward

bindkey '^[?'	run-help		# カーソル下の manを表示。
bindkey '^q^q'	quoted-insert
bindkey '^v'	undefined-key
bindkey '^w'	kill-region

#---------------------------------------------------------------------
# Aliases
#---------------------------------------------------------------------
if [ "$OS_KIND" = Darwin ]; then
  alias emacs=$EMACS
  alias kindlegen=/Applications/Kindle\ Previewer\ 3.app/Contents/MacOS/lib/fc/bin/kindlegen
fi

if exists hub; then
  eval "$(hub alias -s)"
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
alias expandurl="perl -MLWP::UserAgent -lE 'say LWP::UserAgent->new->head(shift)->request->uri'"
alias g=git
alias hall="history -E -i 1"

# Local Variables:
# coding: utf-8
# mode: shell-script
# tab-width: 4
# End:
