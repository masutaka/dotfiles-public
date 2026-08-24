#!/usr/bin/env ruby

# Claude Code の PostToolUse(Write|Edit|Bash) フックから呼ばれ、書き換えられた
# Markdown ファイルに対して mo CLI を実行する薄いラッパ。
#
# 標準入力から PostToolUse のフックペイロード(JSON)を受け取り、対象ファイルを
# 以下のように取り出す:
#   - Write / Edit … tool_input.file_path
#   - Bash         … tool_input.command から書き込み先を抽出
#
# 取り出したファイルのうち、.claude/plans/ 配下(crit が起動して競合するため)と
# メモリファイルを除いた Markdown に対して mo を実行する。
#
# mo の失敗で Claude Code 側の処理を止めないよう、エラーは握りつぶす。
#
# usage: PostToolUse フックから標準入力経由で呼ばれる

require 'json'
require 'shellwords'

HEREDOC = /<<-?\s*['"]?(\w+)/
SEPARATORS = [';', '|', '||', '&&', '&'].freeze
REDIRECT = /\A>>?(.*)\z/

# heredoc の本文を落とす。本文に書かれたコマンド例や引用符で、無関係な
# ファイルを拾ったりトークン化が壊れたりするのを防ぐ。
def without_heredoc_bodies(command)
  kept = []
  delimiter = nil

  command.each_line(chomp: true) do |line|
    if delimiter
      delimiter = nil if line.strip == delimiter
    else
      kept << line
      delimiter = line[HEREDOC, 1]
    end
  end

  kept.join("\n")
end

# Shellwords は改行を単なる空白として扱うため、コマンドの境界が消える。
# 行継続を繋いだうえで、残る改行を区切りに置き換えておく。
def normalize(command)
  without_heredoc_bodies(command)
    .gsub(/\\\n/, ' ')
    .gsub("\n", ' ; ')
end

def tokenize(command)
  Shellwords.split(normalize(command))
rescue ArgumentError
  []
end

# ; や | で区切られたコマンド単位に分ける。
def each_command(tokens)
  tokens.chunk { |token| SEPARATORS.include?(token) }
        .reject { |separator, _| separator }
        .map { |_, segment| segment }
end

# リダイレクト先。`> a.md` と `>a.md` の両方の書き方を拾う。
def redirect_targets(tokens)
  tokens.each_with_index.filter_map do |token, index|
    matched = token.match(REDIRECT)
    next unless matched

    matched[1].empty? ? tokens[index + 1] : matched[1]
  end
end

# sed の in-place 編集と tee はどちらも書き込み系なので、引数を対象にする。
def write_command_targets(tokens)
  in_place = tokens.first == 'sed' && tokens.any? { |token| token.start_with?('-i', '--in-place') }
  return [] unless tokens.first == 'tee' || in_place

  tokens.drop(1).reject { |token| token.start_with?('-') }
end

# 書き込み先を返す。cat や grep の引数に現れた Markdown は拾わない。
# `cat > a.md; ...` のように演算子が空白なしで続くと Shellwords が 1 トークンに
# まとめるため、末尾の演算子を落とす。
def written_paths(command)
  each_command(tokenize(command)).flat_map do |tokens|
    redirect_targets(tokens) + write_command_targets(tokens)
  end.compact.map { |path| path.sub(/[;&|]+\z/, '') }
end

def targets(payload)
  tool_input = payload['tool_input'] || {}

  case payload['tool_name']
  when 'Write', 'Edit' then [tool_input['file_path'].to_s]
  when 'Bash' then written_paths(tool_input['command'].to_s)
  else []
  end
end

def excluded?(path)
  resolved = File.expand_path(path)

  resolved.include?('/.claude/plans/') ||
    (resolved.include?('/.claude/projects/') && resolved.include?('/memory/'))
end

payload = begin
  JSON.parse($stdin.read)
rescue JSON::ParserError
  exit
end

targets(payload).compact.uniq.each do |target|
  next unless File.extname(target) == '.md'
  next unless File.file?(target)
  next if excluded?(target)

  system('mo', target, in: File::NULL, out: File::NULL, err: File::NULL)
end
