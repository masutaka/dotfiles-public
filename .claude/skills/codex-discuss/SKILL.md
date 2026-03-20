---
name: codex-discuss
description: 引数で指定された議題について Codex CLI と議論するスキル。Claude Code と Codex が反復的に議論してより良いアプローチを探し、変更はユーザーに一つずつ確認してから反映する。
disable-model-invocation: true
---

codex と $ARGUMENTS について議論して。

$ARGUMENTS が指定されていない場合は、ユーザーに何を議論したいか確認すること。

## 事前準備

$ARGUMENTS に必要なコンテキスト（コード、計画、会話の要約など）を収集する。

## codex との会話方法

1. $ARGUMENTS と収集したコンテキストを含めて新規セッションを作成する:
   `codex exec --full-auto -o /tmp/codex-session-${CLAUDE_SESSION_ID}.txt "$ARGUMENTS\n\n<収集したコンテキスト>"`
2. 出力から `session id: <UUID>` を取得し、以降の会話で使用する
3. 以降は以下のコマンドで会話を続ける（SESSION_ID は UUID なので複数インスタンスで競合しない）:
   `codex exec resume --full-auto -o /tmp/codex-<SESSION_ID>.txt <SESSION_ID> "会話内容"`
4. codex の応答は対応する出力ファイルから読み取る

## 議論の進め方

- codex の意見に対して、より良い方法がないか codex と議論すること
- 合意が取れたら議論完了とする
- 議論の結果に基づく変更はユーザーにひとつずつ確認してから反映すること
