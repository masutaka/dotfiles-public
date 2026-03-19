---
name: codex-collaborate
description: Collaborate with Codex CLI to review content (e.g. issue plans). Discuss feedback iteratively between Claude Code and Codex to find better approaches, then confirm each change with the user before applying.
disable-model-invocation: true
---

codex にレビューしてもらって。

## codex との会話方法

1. まず `codex exec --full-auto -o /tmp/codex-session.txt "レビューを開始します。レビュー対象の内容を送ってください"` で新規セッションを作成する
2. 出力から `session id: <UUID>` を取得し、以降の会話で使用する
3. 以降は `codex exec resume --full-auto -o /tmp/codex-session.txt <SESSION_ID> "会話内容"` で会話する
4. codex の応答は /tmp/codex-session.txt から読み取る

## レビューの進め方

- レビューへの指摘については、codex と話し合って、より良い方法を模索すること
- 最終的に内容を反映するかは、ユーザーにひとつずつ確認すること
