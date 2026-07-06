#!/usr/bin/env bash

# Claude Code の PostToolUse(Write|Edit) フックから呼ばれ、編集された
# Markdown ファイルに対して mo CLI を実行する薄いラッパ。
#
# 標準入力から PostToolUse のフックペイロード(JSON)を受け取り、
# tool_input.file_path を取り出して以下のように振り分ける:
#   - .claude/plans/*         配下 … 何もしない(crit が起動して競合するため)
#   - .claude/projects/*/memory/* 配下 … 何もしない(メモリファイル)
#   - *.md                          … mo を実行する
#   - それ以外                      … 何もしない
#
# mo の失敗で Claude Code 側の処理を止めないよう、エラーは握りつぶす。
#
# usage: PostToolUse フックから標準入力経由で呼ばれる

f=$(jq -r '.tool_input.file_path // empty') || exit 0

case "$f" in
  */.claude/plans/*) ;;
  */.claude/projects/*/memory/*) ;;
  *.md) mo "$f" </dev/null 2>/dev/null || true ;;
esac
