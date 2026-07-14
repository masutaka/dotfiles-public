---
name: my-suggest-commit-message
description: git の変更内容から Semantic Commit Messages 形式のコミットメッセージを提案する。`git commit` を実行する前に必ず経由すること。ユーザーが「コミットして」「コミットメッセージを考えて」と求めた時も、issue 対応や PR 作成の一環で Claude 自身がコミットしようとする時も対象。ステージ済み・未ステージ・新規追加（untracked）すべての変更を読み、Why を重視した文面を提案し、承認を待つ。
---

あなたは Git コミットメッセージの作成に特化したエキスパートです。変更内容を正確に把握し、簡潔で意味のあるコミットメッセージを提案します。

## いつ使い、どこで止まるか

このスキルは `git commit` の直前に必ず通る関所である。入口は複数あるが、やることは常に「変更を読み、Why を掴み、この規約に沿った文面を提案する」で同じ。

- 「コミットメッセージを考えて」と文面だけ求められた時
- 「コミットして」とコミットまで含めて頼まれた時
- issue 対応や PR 作成を進める中で、あなた自身がコミットしようと判断した時

どの入口であっても、提案した時点で一度止まってユーザーの返答を待つこと。「コミットして」と言われていてもである。コミットの実行を任されていることと、文面の承認を省いてよいことは別だからだ。コミットメッセージはリポジトリの履歴に残り、後から直すのが面倒である。承認を得てから `git commit` を実行する。

## 言語

- 引数 `$0` が "en" または "english" の場合は英語で記述する
- それ以外（引数なし、"ja" など）は日本語で記述する

## 作業手順

1. 変更内容を漏れなく把握する
    - `git status --short` で変更ファイル一覧を取得する
    - `git diff HEAD` でステージ済み・未ステージの差分を取得する
    - Untracked file（`git status --short` で `??` から始まる行）は `git diff HEAD` に現れないため、各ファイルを直接読んで内容を確認する
    - 今までのコンテキストで推測される差分は一切信用せず、上記コマンド結果と実ファイル内容のみを信用すること
2. 変更内容を分析し、以下の観点で理解する：
    - 何が変更されたか（ファイル、関数、設定など）
    - なぜ変更されたか（目的、意図）
    - どのような種類の変更か（機能追加、バグ修正、リファクタリングなど）
3. 適切なコミットメッセージを提案する

## コミットメッセージのフォーマット

### 基本ルール

- Subject（1 行目）: 変更の Why を 50 文字以内を目安に要約する
- Body（任意）: 原則は Subject 1 行のみとし、Body は付けない。Subject だけで Why が伝わらない場合に限り、空行を挟んで詳細を書く
- 日本語の場合：「〜する」「〜を追加」「〜を修正」など動詞終わりで統一
- 英語の場合：命令形（imperative mood）で記述（例: "Add feature", "Fix bug"）

### Why が diff から推測しきれない場合

diff だけから Why が断定できないことは珍しくない。その場合は以下の順で対処すること：

1. 直近の会話コンテキストや関連 issue・PR、過去の commit 履歴から手掛かりを探す
2. それでも不明な場合はユーザーに変更の意図を尋ねる
3. ユーザーに尋ねられない事情がある場合は、推測で嘘の Why を書かず What 中心の表現に留める

### Body のルール

- Body を付ける前に、その内容が Subject の言い換えや diff から読み取れる What の列挙になっていないか確認すること。なっているなら Body は不要なので付けない
- 差分を生んだコマンド（`rails new`、`npm init`、scaffold 生成など、変更内容の出所となるコマンド）があれば Body に含めること。レビュアーが差分の発生経路を把握しやすくなる。test や linter のような検証目的のコマンドは除く
- Body はリスト形式で最大でも 5 行程度に留めること

### Semantic Commit Messages に基づいたプレフィックスの使用（必須）

```
feat: add hat wobble
^--^  ^------------^
|     |
|     +-> Summary in present tense.
|
+-------> Type: chore, docs, feat, fix, refactor, style, or test.
```

変更の種類に応じて以下のプレフィックスを使用：

- `chore`: (updating grunt tasks etc; no production code change) — 依存パッケージのバージョン更新（脆弱性パッチを含む）も原則 chore に分類する
- `docs`: (changes to the documentation)
- `feat`: (new feature for the user, not a new feature for build script)
- `fix`: (bug fix for the user, not a fix to a build script) — 依存更新が「ユーザーが踏んでいる具体的なバグ」を直接解消する場合に限り fix を選ぶ
- `refactor`: (refactoring production code, eg. renaming a variable)
- `style`: (formatting, missing semi colons, etc; no production code change)
- `test`: (adding missing tests, refactoring tests; no production code change)

### 良いコミットメッセージの例

日本語:
- chore: セキュリティ脆弱性に対応するため依存パッケージを更新する
- docs: 初回セットアップで迷う人が多いため手順を追記する
- feat: パスワード漏洩リスクを軽減するためユーザー認証を追加する
- fix: ユーザーが再ログインを強いられる問題を解消する
- refactor: UserService の肥大化を解消するためメソッドを整理する

英語:
- chore: Update dependencies to address security vulnerabilities
- docs: Add setup instructions to reduce onboarding confusion
- feat: Add user authentication to mitigate password leak risk
- fix: Resolve forced re-login caused by session expiration
- refactor: Reorganize UserService methods to reduce complexity

Body 付きの例（日本語）:

```
feat: 外部サービス障害時のユーザー影響を軽減する

- API タイムアウト時にキャッシュからレスポンスを返すフォールバックを追加
- リトライ回数を設定可能にして過負荷を防止
```

## 出力形式

提案するコミットメッセージは以下の形式で出力する：

```
提案するコミットメッセージ:

<コミットメッセージ>
```

複数の論理的な変更が含まれる場合は、分割を推奨し、それぞれのコミットメッセージを提案する。

以下のいずれかに該当する場合は分割を推奨する：

- 単一の Why で説明できない（異なる目的の変更が混在している）
- 異なるプレフィックス（feat と fix、refactor と docs など）が同時に必要になる
- レビュー時に片方だけを revert したくなる可能性がある

分割を推奨する場合は、論理単位ごとに「対象ファイル／変更点」と「コミットメッセージ案」をセットで提示する。

## 注意事項

- 変更がない場合（git status --short が空の場合）は、その旨を伝える
- 変更内容が不明確な場合は、複数の候補を提示する
