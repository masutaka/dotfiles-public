#!/usr/bin/env bash

set -euo pipefail

# 同一 tmux ウィンドウ内の複数 pane で Claude Code を同時起動することは非対応。
# 状態ファイルは pane ごとに分かれるが、ウィンドウ名は共通なので複数のスピナー
# が 0.1 秒ごとに rename-window を奪い合って表示が壊れる。

# tmux 外では何もしない
[[ -z "${TMUX:-}" ]] && exit 0

PANE_ID="${TMUX_PANE}"
WINDOW_ID=$(tmux display-message -t "$PANE_ID" -p '#{window_id}' 2>/dev/null || echo '')
SUFFIX="${PANE_ID//[^a-zA-Z0-9]/_}"
TMPDIR="${TMPDIR:-/tmp}"
PID_FILE="${TMPDIR%/}/claude-tmux-spinner-${SUFFIX}.pid"
NAME_FILE="${TMPDIR%/}/claude-tmux-spinner-${SUFFIX}.name"
HEARTBEAT_FILE="${TMPDIR%/}/claude-tmux-spinner-${SUFFIX}.heartbeat"
HEARTBEAT_TTL_SEC=180

# バックグラウンドで回っているスピナー（rename-window のループ）を停止する補助関数。
kill_spinner() {
  if [[ -f "$PID_FILE" ]]; then
    kill "$(cat "$PID_FILE")" 2>/dev/null || true
    rm -f "$PID_FILE"
  fi
}

# ハートビートファイルを現在時刻で原子的に書き込む補助関数。`> file` で直接書
# くとスピナーループの cat との間で O_TRUNC → write のレース窓が生まれ、空ファ
# イルを読むと空文字列が算術展開で 0 扱いされて自壊条件を誤って満たしてしまう。
# tmp ファイルに書いて mv で置き換えれば rename は POSIX 上原子的で、読み手は
# 旧値か新値のどちらかを必ず見る。
write_heartbeat() {
  local tmp="${HEARTBEAT_FILE}.tmp"
  date +%s > "$tmp" && mv "$tmp" "$HEARTBEAT_FILE"
}

# ハートビートファイルを更新する補助関数。Claude Code がツールを使っている間
# はこれで定期的にリフレッシュされ、スピナーは TTL 超過による自壊を回避する。
# 逆に Esc 中断時はこの更新が止まり、スピナーが自壊する。
refresh_heartbeat() {
  [[ -f "$HEARTBEAT_FILE" ]] && write_heartbeat
}

# Claude Code が新しいリクエストの処理を始めたときに、未読フラグをクリアし、
# 元のウィンドウ名を保存してから、ブライユ点字スピナーをバックグラウンドで回し始める。
start_spinner() {
  [[ -z "$WINDOW_ID" ]] && exit 0

  # 未読フラグをクリア（新しいリクエスト開始）。tmux.conf の after-select-window
  # フックが通常はクリアするが、ウィンドウを切り替えずに同じウィンドウ内で続けて
  # プロンプトを送信するケース用の安全網として明示的に unset する。
  tmux set-option -w -t "$WINDOW_ID" -u @unread 2>/dev/null || true

  kill_spinner

  # 元のウィンドウ名を保存（未保存または空ファイルの場合のみ）
  if [[ ! -s "$NAME_FILE" ]]; then
    tmux display-message -t "$WINDOW_ID" -p '#{window_name}' > "$NAME_FILE" 2>/dev/null
  fi
  local original_name
  original_name=$(cat "$NAME_FILE")

  # ハートビートを初期化（後続の Pre/PostToolUse フックで更新される）
  write_heartbeat

  # スピナーをバックグラウンドで開始（ウィンドウ ID を明示的に指定）
  (
    SPINNER=('⠋' '⠙' '⠹' '⠸' '⠼' '⠴' '⠦' '⠧' '⠇' '⠏')
    i=0
    while true; do
      # Esc 中断などで heartbeat が止まって TTL を超えたら自壊する。
      # 元のウィンドウ名を復元し、関連する一時ファイルを掃除してから抜ける。
      heartbeat_ts=$(cat "$HEARTBEAT_FILE" 2>/dev/null || echo 0)
      # 万一空文字列を読んだ場合（write_heartbeat が mv で原子化しているので
      # 通常は起きないが防御的に）は「直近で書かれた」扱いで自壊を回避する。
      [[ -z "$heartbeat_ts" ]] && heartbeat_ts=$(date +%s)
      if (( $(date +%s) - heartbeat_ts > HEARTBEAT_TTL_SEC )); then
        tmux rename-window -t "$WINDOW_ID" "$original_name" 2>/dev/null || true
        rm -f "$NAME_FILE" "$HEARTBEAT_FILE" "$PID_FILE"
        break
      fi
      tmux rename-window -t "$WINDOW_ID" "${SPINNER[$i]} ${original_name}" 2>/dev/null || break
      i=$(( (i + 1) % ${#SPINNER[@]} ))
      sleep 0.1
    done
  ) > /dev/null 2>&1 &
  local pid=$!
  disown "$pid"
  echo "$pid" > "$PID_FILE"
}

# Claude Code が応答を中断してユーザーの判断を待つとき（ファイル編集やコマンド
# 実行の許可、アイドル待ちなど）に呼ばれる。スピナーは止めず、別ウィンドウにい
# た場合のみ未読フラグを立てる。
notify_attention() {
  [[ -z "$WINDOW_ID" ]] && exit 0

  local current_window
  current_window=$(tmux display-message -p '#{window_id}' 2>/dev/null || echo '')
  if [[ "$current_window" != "$WINDOW_ID" ]]; then
    tmux set-option -w -t "$WINDOW_ID" @unread 1 2>/dev/null || true
  fi
}

# Claude Code が応答を完了したときに、スピナーを停止してウィンドウ名を元に戻し、
# 別ウィンドウにいた場合のみ未読フラグを立てる（同じウィンドウを見ていれば既に
# 視認済みなので立てない）。
stop_spinner() {
  kill_spinner

  [[ -z "$WINDOW_ID" ]] && exit 0

  # 元のウィンドウ名を復元（空ファイルなら復元すべき名前が無いのでスキップ）
  if [[ -s "$NAME_FILE" ]]; then
    local original_name
    original_name=$(cat "$NAME_FILE")
    tmux rename-window -t "$WINDOW_ID" "$original_name" 2>/dev/null || true
    rm -f "$NAME_FILE"
  fi
  rm -f "$HEARTBEAT_FILE"

  # 別のウィンドウにいる場合のみ未読フラグを設定
  local current_window
  current_window=$(tmux display-message -p '#{window_id}' 2>/dev/null || echo '')
  if [[ "$current_window" != "$WINDOW_ID" ]]; then
    tmux set-option -w -t "$WINDOW_ID" @unread 1 2>/dev/null || true
  fi
}

case "${1:-}" in
  start)     start_spinner ;;
  heartbeat) refresh_heartbeat ;;
  notify)    notify_attention ;;
  stop)      stop_spinner ;;
  *) echo "usage: $0 start|heartbeat|notify|stop" >&2; exit 2 ;;
esac
