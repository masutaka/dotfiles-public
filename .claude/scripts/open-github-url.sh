#!/usr/bin/env bash

# Claude Code の PostToolUse(Bash) フックから呼ばれ、gh CLI で GitHub の
# issue / pr / discussion を「作成」した直後に、その URL をブラウザで開く薄い
# ラッパ。
#
# 標準入力から PostToolUse のフックペイロード(JSON)を受け取り、
# tool_input.command が作成系コマンドの時だけ、tool_response 中に現れる
# GitHub の issue/pull/discussion URL を取り出してブラウザで開く。
#   - gh issue create / gh pr create / gh discussion create
#   - gh api graphql の createDiscussion mutation
# view や list など参照系コマンドの URL は開かない。
#
# open の失敗で Claude Code 側の処理を止めないよう握りつぶす。
#
# usage: PostToolUse(Bash) フックから標準入力経由で呼ばれる

input=$(cat)

command=$(printf '%s' "$input" | jq -r '.tool_input.command // empty') || exit 0

# 作成系コマンドでなければ何もしない
printf '%s' "$command" | grep -qE 'gh (issue|pr|discussion) create|createDiscussion' || exit 0

url=$(printf '%s' "$input" \
  | jq -r '.tool_response | tostring' \
  | grep -oE 'https://github\.com/[A-Za-z0-9._-]+/[A-Za-z0-9._-]+/(issues|pull|discussions)/[0-9]+' \
  | head -1)

[ -z "$url" ] && exit 0

open "$url" 2>/dev/null || true
