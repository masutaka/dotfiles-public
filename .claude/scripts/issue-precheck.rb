#!/usr/bin/env ruby

# Claude Code の UserPromptSubmit フックから呼ばれ、プロンプトに GitHub Issue の
# URL が含まれていたら、着手前の前提検証チェックを context に注入する。
#
# Issue は書かれた時点のスナップショットでしかない。時間が経って前提が変わって
# いたり、What だけ書かれて Why が抜けていたりする。それを仕様として鵜呑みにした
# まま実装が進むと手戻りになるため、着手前に必ず検証させる。
#
# 発火は Issue URL のみ。#123 のような番号だけの記法は PR 番号やコミット
# メッセージの引用でも当たってしまうため、対象にしない。
#
# フックの失敗で Claude Code 側の処理を止めないよう、エラーは握りつぶす。
#
# usage: UserPromptSubmit フックから標準入力経由で呼ばれる

require 'json'

ISSUE_URL = %r{https?://github\.com/[\w.-]+/[\w.-]+/issues/\d+}

CHECKLIST = <<~TEXT
  プロンプトに GitHub Issue の URL が含まれている。Issue は書かれた時点の
  スナップショットであり、そのまま仕様として信じると手戻りになる。実装に着手
  する前に以下を検証すること。

  1. 鮮度: Issue の作成日と最終更新日を確認し、それ以降に関連するコミットや PR が
     ないか調べる。Issue が書かれた頃と状況が変わっていないか
  2. 裏取り: Issue に書かれた前提(ファイルパス、挙動、原因の見立て)をコードで
     確認する。Issue の記述を仕様として鵜呑みにしない
  3. Why: Issue に Why が書かれているか確認する。What だけの場合は Why を推測して
     実装を始めず、自分の理解を明示してユーザーに確認する

  いずれかで前提のズレを見つけたら、作業を進めずユーザーに報告すること。
TEXT

begin
  payload = JSON.parse($stdin.read)
  exit 0 unless payload['prompt'].to_s.match?(ISSUE_URL)

  puts JSON.generate(
    hookSpecificOutput: {
      hookEventName: 'UserPromptSubmit',
      additionalContext: CHECKLIST
    }
  )
rescue StandardError
  exit 0
end
