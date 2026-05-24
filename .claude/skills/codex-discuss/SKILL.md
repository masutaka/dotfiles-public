---
name: codex-discuss
description: 論点整理と反証のために Codex CLI と議論する。複数案のトレードオフを比較したい、自案への反証や弱点を検証したい、判断基準を確認したいときに使う。実装やファイル変更は一切行わず、議論と最終報告のみ
---

codex と $ARGUMENTS について議論して。

$ARGUMENTS が指定されていない場合は、ユーザーに何を議論したいか確認すること。

## このスキルを使う場面

- 複数の案があり、トレードオフを整理したいとき
- 自分の案に対して反証や弱点の検証をしたいとき
- 既存の制約を踏まえた判断が必要なとき

## このスキルを使わない場面

- 仕様や事実の確認だけで済むとき
- 明確な対処手順が既にあるとき
- 単純な生成や作業代行だけが目的で、論点整理やトレードオフ検討が不要なとき

## 事前準備

$ARGUMENTS を元に、必要なコンテキストを収集する。分かっている範囲で以下を含める:

- 議論したい論点
- 背景や制約
- 現時点で考えている案や迷い
- 特に見てほしい点
- 判断基準（何を優先して結論を出したいか）

## codex との会話方法

このスキルで使うファイルパスは以下で固定する。`CLAUDE_CODE_SESSION_ID` はセッション間の衝突防止用（未設定時は `default`）:

- プロンプトファイル: `/tmp/codex-discuss-${CLAUDE_CODE_SESSION_ID:-default}-prompt.txt`
- 応答出力ファイル: `/tmp/codex-discuss-${CLAUDE_CODE_SESSION_ID:-default}-out.txt`

### 呼び出しの絶対ルール

以下は長文プロンプトで Codex が応答を返さない事故を防ぐために必須:

- 議論文は必ず一時ファイルに書き、`-` 引数で stdin から渡す。コマンドライン引数で直接渡すと、改行・引用符・コードブロックを含む長文でシェルのクォーティングが壊れ、Codex が壊れたプロンプトを受け取って無応答や誤応答になる
- Bash 呼び出しには必ず `timeout: 600000`（10 分）を指定する。Bash のデフォルトは 2 分で、中規模プロンプトでも Codex の応答に 60 秒以上かかるため、長文では Bash 側でタイムアウトして殺される
- このスキルは議論専用なので、初回 `codex exec` には必ず `--sandbox read-only` を付ける。`codex exec resume` は初回セッションの sandbox を継承する仕様なので、`--sandbox` や非推奨の `--full-auto` を resume 側で指定してはいけない
- 議論内容はリポジトリの中身に依存しない汎用議論もあり得るので、`--skip-git-repo-check` を常に付ける

### 1. 新規セッションの開始

```bash
cat > /tmp/codex-discuss-${CLAUDE_CODE_SESSION_ID:-default}-prompt.txt <<'EOF'
<議論内容とコンテキスト>
EOF

cat /tmp/codex-discuss-${CLAUDE_CODE_SESSION_ID:-default}-prompt.txt | \
  codex exec --json --sandbox read-only --skip-git-repo-check \
    -o /tmp/codex-discuss-${CLAUDE_CODE_SESSION_ID:-default}-out.txt -
```

### 2. 応答の取得と成否の検証

応答本文は出力ファイル `/tmp/codex-discuss-${CLAUDE_CODE_SESSION_ID:-default}-out.txt` から読む。標準出力は JSONL イベントストリームで、以下を確認する:

- `thread.started` の `thread_id` を控える（以降の resume で必要）
- `turn.completed` が含まれていて、かつ出力ファイルが非空ならば成功
- `turn.failed` が含まれている、または `turn.completed` が無いまま終わっている、または出力ファイルが空ならば失敗

失敗時は JSONL の末尾と stderr の主要な行を添えてユーザーに報告し、議論を継続しない。

### 3. セッションの継続

`<thread_id>` は新規セッションで控えた値:

```bash
cat > /tmp/codex-discuss-${CLAUDE_CODE_SESSION_ID:-default}-prompt.txt <<'EOF'
<次のメッセージ>
EOF

cat /tmp/codex-discuss-${CLAUDE_CODE_SESSION_ID:-default}-prompt.txt | \
  codex exec resume --json --skip-git-repo-check \
    -o /tmp/codex-discuss-${CLAUDE_CODE_SESSION_ID:-default}-out.txt \
    <thread_id> -
```

成否の判定方法は新規セッションと同じ。

## 議論の進め方

このスキルは議論・論点整理専用であり、実装やファイル変更、設定変更などの反映作業は行わない。

- まず自分の仮説を提示し、codex に反証を求める
- 一方的に同意せず、根拠を求めたり代替案を提示すること
- 必要に応じて、実現可能性・コスト・影響範囲・既存の仕組みとの整合性などの観点から議論する
- 以下のいずれかで議論を終了する（通常 2〜5 往復、ただし重要な未解決論点が残る場合は延長可）:
  - 結論が定まった場合
  - 主要なトレードオフが整理できた場合
  - これ以上深掘りしても有益な進展が見込みにくい場合

## 議論のまとめ

議論完了後、ユーザーに以下の構成で報告すること:

1. 議論の経緯と明らかになったこと（どのような選択肢が出て、何が論点になり、どう結論に至ったか）
2. 未解決の論点
3. 提案するアクション（あれば）

結論を実際に反映する場合は、このスキルの外でユーザー確認のうえ実施すること。
