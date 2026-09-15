#!/usr/bin/env ruby

# Claude Code の PostToolUse(Write|Edit) フックから呼ばれ、Markdown に書き込んだ
# 文章を textlint-rule-preset-ai-words-ja で検査する薄いラッパ。
#
# 標準入力から PostToolUse のフックペイロード(JSON)を受け取り、検査対象を
# 以下のように取り出す:
#   - Write … tool_input.content
#   - Edit  … tool_input.new_string
#
# ファイル全体ではなく書いた部分だけを検査する。既存の文章への指摘で、
# 頼んでいない修正が始まるのを防ぐため。
#
# textlint とプリセットは npx で実行する。バージョンは固定しないので、
# min-release-age を満たす最新版が使われる。条件を満たす版がなければ
# npx が失敗し、検査は行われない。
#
# プリセットは 2026-09-12 に公開されたばかりで、~/.config/npm/config の
# min-release-age=7 を満たす版がまだない。v1.2.0 が 7 日を超える 2026-09-20
# までは、一時的に --min-release-age=2 で上書きする。
#
# 指摘があれば stderr に出して exit 2 で Claude に返す。npx の失敗など、
# 指摘以外の失敗では Claude Code 側の処理を止めない。
#
# usage: PostToolUse フックから標準入力経由で呼ばれる

require 'json'
require 'open3'

LINT_ERROR = 1

def written_text(payload)
  tool_input = payload['tool_input'] || {}

  case payload['tool_name']
  when 'Write' then tool_input['content'].to_s
  when 'Edit' then tool_input['new_string'].to_s
  else ''
  end
end

payload = begin
  JSON.parse($stdin.read)
rescue JSON::ParserError
  exit
end

path = payload.dig('tool_input', 'file_path').to_s
exit unless File.extname(path) == '.md'

text = written_text(payload)
exit if text.empty?

command = ['npx', '--yes', '--min-release-age=2', '-p', 'textlint', '-p', 'textlint-rule-preset-ai-words-ja',
           'textlint', '--no-textlintrc', '--preset', 'ai-words-ja',
           '--stdin', '--stdin-filename', File.basename(path)]

output, _error, status = begin
  Open3.capture3(*command, stdin_data: text)
rescue SystemCallError
  exit
end

# npx 自体の失敗も exit 1 になる。その場合 stdout は空なので、指摘と区別できる。
exit unless status.exitstatus == LINT_ERROR && !output.empty?

warn "#{path} に書いた部分を textlint で検査しました。行番号は書いた部分の中での位置です。"
warn output
exit 2
