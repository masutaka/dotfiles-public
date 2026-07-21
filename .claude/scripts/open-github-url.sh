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
# 1 回の Bash 呼び出しで複数作成された場合に備え、重複を除いた全 URL を開く。
# open の失敗で Claude Code 側の処理を止めないよう握りつぶす。
#
# usage: PostToolUse(Bash) フックから標準入力経由で呼ばれる

input=$(cat)

command=$(printf '%s' "$input" | jq -r '.tool_input.command // empty') || exit 0

# 作成系コマンドでなければ何もしない
printf '%s' "$command" | grep -qE 'gh (issue|pr|discussion) create|createDiscussion' || exit 0

# tool_response は文字列の場合とオブジェクト({stdout,stderr,...})の場合がある。
# tostring で JSON 化すると内部の改行が \n にエスケープされ複数 URL が連結して
# しまうため、文字列はそのまま、オブジェクトは全 leaf 文字列を取り出す。
urls=$(printf '%s' "$input" \
  | jq -r '.tool_response | if type == "string" then . else (.. | strings) end' \
  | grep -oE 'https://github\.com/[A-Za-z0-9._-]+/[A-Za-z0-9._-]+/(issues|pull|discussions)/[0-9]+' \
  | awk '!seen[$0]++')

[ -z "$urls" ] && exit 0

printf '%s\n' "$urls" | while IFS= read -r url; do
  [ -n "$url" ] && open "$url" 2>/dev/null || true
done
