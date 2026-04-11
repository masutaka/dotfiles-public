#!/usr/bin/env bash

# macOS 通知センターに「Claude Code」+「確認してください」の通知を出す薄いラッパ。
# tmux 内で呼ばれた場合は (tmux:N) 形式でウィンドウ番号を末尾に追記する。
#
# sound は /System/Library/Sounds/*.aiff のファイル名から拡張子を除いたもの
# （例: Glass, Funk, Ping, Tink）。省略時は Glass にフォールバックする。
#
# usage: macos-notify.sh [sound]

set -euo pipefail

sound="${1:-Glass}"

suffix=""
if [[ -n "${TMUX:-}" ]]; then
  window_index=$(tmux display-message -t "${TMUX_PANE}" -p '#{window_index}' 2>/dev/null || echo '')
  [[ -n "$window_index" ]] && suffix=" (tmux:$window_index)"
fi

osascript -e "display notification \"確認してください${suffix}\" with title \"Claude Code\" sound name \"$sound\""
