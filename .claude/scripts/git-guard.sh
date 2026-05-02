#!/usr/bin/env bash

# Claude Code の PreToolUse hook。Bash ツールで実行される git コマンドを検査し、
# 危険なオプションが含まれている場合はブロックする。
#
# permission の deny は前方一致 (`Bash(git commit --amend:*)`) なので、
# `git commit -m "..." --amend` のようにオプション順序を変えると素通りする。
# その抜け道を塞ぐための物理的な保険。
#
# ブロック対象:
#   - git add -A / --all / .   （巨大ファイルや .env を巻き込むリスク）
#   - git commit --amend       （履歴改変は手動でやる方針）
#   - git commit --no-verify / -n （pre-commit hook の skip は禁止）
#
# 設計メモ:
#   - `git -C <path> commit ...` のような global option を許容する
#   - `-nm` / `-mn` などの結合短オプションも検査する
#   - 引用符 ("..." / '...') の中身は placeholder 1 トークンに置換し、
#     message 本文の語で誤検知しないようにしつつトークン整列を保つ
#     (`-c "k=v with space"` のような quote 内空白で位置がズレるのを防ぐ)
#
# stdin: PreToolUse hook の JSON
# exit 0: 通過 / exit 2: ブロック (stderr の内容が Claude に渡る)

set -euo pipefail

input=$(cat)
tool_name=$(printf '%s' "$input" | jq -r '.tool_name // ""')

if [[ "$tool_name" != "Bash" ]]; then
  exit 0
fi

command=$(printf '%s' "$input" | jq -r '.tool_input.command // ""')

# git の前置 global option で、次トークンを引数として消費するもの
git_global_opt_takes_arg() {
  case "$1" in
    -C|-c|--exec-path|--git-dir|--work-tree|--namespace|--super-prefix|--config-env)
      return 0 ;;
    *)
      return 1 ;;
  esac
}

block() {
  echo "git-guard: $1" >&2
  exit 2
}

check_segment() {
  local segment="$1"

  # quote の中身を placeholder 1 トークン (__Q__) に置換する。
  # message 本文の語による false positive を避けつつ、quote 内空白で
  # トークン整列が崩れて global option の arg 検出がズレるのを防ぐ。
  # (例: `git -c "k=v with space" commit --amend` を取りこぼさない)
  # エスケープ済み quote は無視するが、実用上の誤検知は十分減らせる。
  segment=$(printf '%s' "$segment" \
    | sed -E 's/"[^"]*"/__Q__/g' \
    | sed -E "s/'[^']*'/__Q__/g")

  local -a tokens=()
  read -ra tokens <<<"$segment"
  (( ${#tokens[@]} >= 1 )) || return 0
  [[ "${tokens[0]}" == "git" ]] || return 0

  # global option をスキップしてサブコマンドを取得
  local i=1
  local n=${#tokens[@]}
  while (( i < n )); do
    local t="${tokens[i]}"
    if git_global_opt_takes_arg "$t"; then
      i=$((i + 2))
    elif [[ "$t" == -* ]]; then
      i=$((i + 1))
    else
      break
    fi
  done

  (( i < n )) || return 0
  local subcmd="${tokens[i]}"
  local -a sub_args=("${tokens[@]:i+1}")

  case "$subcmd" in
    add)
      local a
      for a in "${sub_args[@]:-}"; do
        case "$a" in
          -A|--all|.|./)
            block "'git add -A / --all / . / ./' は禁止。ファイル名を明示してステージしてください。" ;;
          --*) ;;
          -*)
            # 結合短オプション内に A があるか (-Av, -vA, -An, ...)
            if [[ "$a" == *A* ]]; then
              block "'git add -A' (結合短オプション) は禁止。ファイル名を明示してステージしてください。"
            fi
            ;;
        esac
      done
      ;;
    commit)
      local a
      for a in "${sub_args[@]:-}"; do
        case "$a" in
          --amend|--amend=*)
            block "'git commit --amend' は禁止。新しいコミットを作成してください。" ;;
          --no-verify)
            block "'git commit --no-verify' は禁止。hook の失敗は根本原因を修正してください。" ;;
          --*) ;;
          -*)
            # 結合短オプション内に n があるか (-n, -nm, -mn, ...)
            if [[ "$a" == *n* ]]; then
              block "'git commit -n' (--no-verify) は禁止。hook の失敗は根本原因を修正してください。"
            fi
            ;;
        esac
      done
      ;;
  esac
}

# `;` `&&` `||` `|` で連結された各セグメントを順に検査
# (末尾改行のない入力でも最終行を取りこぼさないよう || [[ -n ]] を併用)
while IFS= read -r segment || [[ -n "$segment" ]]; do
  trimmed=$(printf '%s' "$segment" | sed -E 's/^[[:space:]]+|[[:space:]]+$//g')
  [[ -z "$trimmed" ]] && continue
  check_segment "$trimmed"
done < <(printf '%s\n' "$command" | sed -E 's/[;&|]+/\n/g')

exit 0
