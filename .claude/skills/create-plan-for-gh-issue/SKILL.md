---
name: create-plan-for-gh-issue
description: ghro CLI で Issue を調査し、tmp/docs/plan-for-issue-{Issue番号}.md に作業計画を作成する
disable-model-invocation: true
argument-hint: [GitHub Issue URL or Issue番号]
---

ghro CLI で $0（GitHub Issue URL または Issue 番号）をコメント含めて熟読し、作業計画を `tmp/docs/plan-for-issue-{Issue番号}.md` として作成して。

$0 が指定されていない場合は、ユーザーに GitHub Issue URL または Issue 番号を確認すること。

## 準備

リポジトリと Issue 番号の特定:

- GitHub Issue URL が渡された場合は URL から `owner/repo` と Issue 番号をパースし、ghro に `--repo owner/repo` で渡す
- Issue 番号だけが渡された場合は cwd の git リポジトリの origin から `owner/repo` を判定する。判定できなければユーザーに確認する

## 情報収集

ghro コマンド例（`--json` で必要なフィールドを指定する。素の `view` は Projects 権限エラーになるため使わない）:

- 本体とコメント: `ghro issue view <番号> --repo owner/repo --json number,title,body,labels,state,comments`
- 関連 PR やリンク先が必要な場合は、上記出力に含まれる URL を起点に追加で `ghro` を呼ぶ

その他に確認すること:

- `docs/` や `tmp/docs/` に既存の作業計画があれば読んで、見出し構成や粒度を参考にする
- 一次情報である公式ドキュメントを確認する（context7 MCP Server を活用）
- 検討事項を挙げる場合は、調査結果に基づく推奨方針も併記する（判断材料なしで読み手に丸投げしない）

### 画像の読み取り

Issue 本文やコメントに画像が含まれる場合（`<img src="...">` や `![...](URL)` 形式）、内容を計画に反映するため以下の手順で取得する:

1. ghro の出力から画像 URL を抽出する
2. `mktemp -d` で一時ディレクトリを作成する
3. gh api で一時ディレクトリにダウンロードする（ghro api ではバイナリ取得ができないため gh api を使う）
    - 例: `gh api "画像URL" --header "Accept: application/octet-stream" > "$tmpdir/1.png"`
4. Read ツールで画像ファイルを読み取り、内容を把握する
5. 計画への反映が終わったら一時ディレクトリを削除する

## 計画作成の方針

Phase の定義:

- 作業手順を Phase で分割する。Phase が 1 つで済む場合は分割不要
- 各 Phase は atomic（最小単位）な変更で、1 コミットに対応する想定にする。「Phase 数を減らすために 1 Phase に複数の変更を詰める」のは NG。Phase 数より atomic 性を優先する
- 各 Phase は独立してレビュー・revert できる単位にする
- Phase の中の判断余地は残してよい。「方針の判断」は計画段階で潰し、「コードの細部」は実装に委ねる

スコープ管理 (YAGNI):

- Issue が要求していない機能を独自に追加しない。「ついでに〜も直す」「将来必要そうだから〜も入れる」は別 Issue として扱う
- 計画段階で気付いた拡張案は「スコープ外」セクションに「将来扱う候補」として列挙し、本計画の Phase には入れない

コンパクトさ:

- 初版は背景・ゴール・Phase 構成が伝わる最小サイズに留める
- 計画は議論と意思決定の土台であり、実装書ではない。細部を書き込みすぎない

## ソースコードの変更を伴う場合

- Issue に関連する既存コードを調査し、構造を把握する
- コードへの参照は GitHub permalink（HEAD のコミットハッシュ付き）で記載し、読み手が裏取りできるようにする
- 技術的な主張（「コールバック X が発火する」等）には、根拠となるコードの permalink を付ける
- 具体的なコードの記載は必要最低限に留める
- 各 Phase はテストが Green な状態で終わるようにする

## 出力形式

`tmp/docs/plan-for-issue-{Issue番号}.md` に以下の最小骨格で出力する。Issue の性質に応じて見出しの追加・削除はしてよい。

````markdown
# Issue #{Issue番号}: {Issueタイトル}

## 背景

{Issue で語られている課題と、計画を立てるに当たって把握した前提}

## ゴール

{この計画を完遂したときに達成される状態}

## Phase 1: {Phase の名前}

{atomic な変更 1 つ分の作業内容と確認事項}

## Phase 2: {Phase の名前}

{atomic な変更 1 つ分の作業内容と確認事項}

(以下、Phase が必要なだけ続く。Phase 数より atomic 性を優先する)

## スコープ外

{Issue が要求していないが計画段階で気付いた拡張案。なければこのセクション自体削除}
````

## 注意事項

- このスキルは計画作成のみを行い、実装やコミットは行わない
- 計画ファイル作成後、ユーザーから明示の指示があるまで次のアクションに進まない
