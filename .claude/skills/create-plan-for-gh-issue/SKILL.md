---
name: create-plan-for-gh-issue
description: ghro CLI で Issue を調査し、tmp/docs/plan-for-issue-{Issue番号}.md に作業計画を作成する
disable-model-invocation: true
argument-hint: [GitHub Issue URL or Issue番号]
---

ghro CLI で $0（GitHub Issue URL または Issue 番号）をコメント含めて熟読し、作業計画を `tmp/docs/plan-for-issue-{Issue番号}.md` として作成して。

$0 が指定されていない場合は、ユーザーに GitHub Issue URL または Issue 番号を確認すること。

## 想定読者と用途

この計画ファイルは、最終的に対象 Issue 自身へのコメントとして投稿される前提で書く。これは書き方に直接影響する:

- 対象 Issue 自身への参照は裸の `#N`（例 `#316`）にしない。GitHub 上で他 Issue を指す自動リンクになり読み手を混乱させる。代わりに「本 Issue」と書く
- 他 Issue / PR（前回対応、関連タスク、並走作業）は裸の `#N` で参照する。GitHub が自動リンクする
- 「対象 Issue: ...」のような自己メタ情報は冒頭に書かない。読み手は自分が今どの Issue にいるか分かっている。代わりに「本 Issue の {引用しているセクション名} に対応する作業計画」のような導入にする

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

### 外部の一次ソースを取りに行く

Issue が外部システム / ライブラリ / CVE / ベンダー製品の変更に依存している場合、二次情報のブログまとめではなく一次ソースに当たる。判定の根拠が一段強くなり、計画の妥当性レビューが速くなる。

代表的な一次ソース:

- CVE / security advisory: NVD (`https://nvd.nist.gov/vuln/detail/CVE-...`)、ベンダー公式 advisory（F5, GitHub Security Advisories, etc.）
- 上流の修正コミット: 修正版 release tag のリリースノートからリンクされる commit。コミットメッセージに発火条件と PoC が記載されることが多く、二次情報の要約より厳密
- リリースノート / CHANGES: 影響範囲と修正範囲の境界が分かる
- 上流リポジトリの該当 issue / PR: 議論経緯と未解決の論点が分かる

一次ソースから引いた重要な記述は、計画に短い原文引用で残す。要約だけにすると検証可能性が落ちる。

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
- 条件付き Phase を認める。先行 Phase の結果次第で後続 Phase 自体が不要になる構成（例: 「Phase 1 影響調査 → 該当しなければここで終了。該当した場合のみ Phase 2 以降を実施」）はそのまま記述する。「念のため全 Phase 実施」と書き換えると過剰スコープになる

スコープ管理 (YAGNI):

- Issue が要求していない機能を独自に追加しない。「ついでに〜も直す」「将来必要そうだから〜も入れる」は別 Issue として扱う
- 計画段階で気付いた拡張案は「スコープ外」セクションに「将来扱う候補」として列挙し、本計画の Phase には入れない

コンパクトさ:

- 初版は背景・ゴール・Phase 構成が伝わる最小サイズに留める
- 計画は議論と意思決定の土台であり、実装書ではない。細部を書き込みすぎない

## リポジトリ内のファイルへの参照

コードに限らず、設定 / template / docs / asset / ディレクトリも含めて、リポジトリ内の何かに言及するときは GitHub permalink（コミットハッシュ付き）で書く。読み手が即時に裏取りできる状態にするのが目的。

- ファイル: `https://github.com/<owner>/<repo>/blob/<HASH>/path/to/file`
- 行 / 行範囲: `...blob/<HASH>/path/to/file#L8` / `...#L8-L12`
- ディレクトリ: `https://github.com/<owner>/<repo>/tree/<HASH>/path/to/dir`

`<HASH>` は計画作成時点のデフォルトブランチ（master / main）の HEAD を使う。短縮形ではなくフルハッシュを使う。作業ブランチの HEAD を使うと rebase / squash merge で URL が壊れるが、デフォルトブランチなら commit が消えないため安定する。

参照を書く前に、その path / 行が本当に実在することを確認する（`ls` や Read で確認）。Issue 本文に書かれたパスは誤記の可能性があり、計画にコピペすると伝言ゲームで誤情報が広がる。

ソースコードに関わる Phase 固有の注意:

- Issue に関連する既存コードを調査し、構造を把握する
- 技術的な主張（「コールバック X が発火する」等）には、根拠となるコードの permalink を付ける
- 具体的なコードの記載は必要最低限に留める
- 各 Phase はテストが Green な状態で終わるようにする

## GitHub の自動リンク識別子は装飾しない

GitHub のコメント / Issue / PR 本文では、以下の識別子が裸で書くだけで自動リンクになる。markdown のリンク記法 (`[#123](https://...)`) で囲むと表示が重複し読みづらい:

- Issue / PR 番号: `#123`、別リポジトリは `owner/repo#123`
- commit hash: `<hash>` / 別リポジトリは `owner/repo@<hash>`
- user mention: `@username`
- CVE / GHSA: `CVE-2026-9256`, `GHSA-xxxx-xxxx-xxxx`

リポジトリ内のファイル / ディレクトリ / 行範囲は自動リンクされないので、上記「リポジトリ内のファイルへの参照」に従い明示的に markdown リンクで書く。

## 出力形式

`tmp/docs/plan-for-issue-{Issue番号}.md` に以下の最小骨格で出力する。Issue の性質に応じて見出しの追加・削除はしてよい。

最初の見出しは Issue タイトルではなく「{何の計画か}」が分かる短いタイトルにする。投稿先 Issue の中で読まれる前提なので、Issue 番号やタイトルの重複は不要。

````markdown
# {作業対象を端的に表すタイトル}

本 Issue の {引用するセクション名 / 該当する論点} に対応する作業計画。

## 背景

{Issue で語られている課題と、計画を立てるに当たって把握した前提}

## ゴール

{この計画を完遂したときに達成される状態}

## Phase 1: {Phase の名前}

{atomic な変更 1 つ分の作業内容と確認事項}

(条件付きの場合は「{先行 Phase の結果} の場合のみ実施」「該当しなければここで終了」を明記)

## Phase 2: {Phase の名前}

{atomic な変更 1 つ分の作業内容と確認事項}

(以下、Phase が必要なだけ続く。Phase 数より atomic 性を優先する)

## スコープ外

{Issue が要求していないが計画段階で気付いた拡張案。なければこのセクション自体削除}

## 参考

- 関連 Issue / PR は裸の `#N` で記載（前回対応、分割タスク、並走作業など）
- 外部の一次ソースは markdown リンクで記載（NVD / ベンダー advisory / 上流 commit / リリースノートなど）
````

## 注意事項

- このスキルは計画作成のみを行い、実装やコミットは行わない
- 計画ファイル作成後、ユーザーから明示の指示があるまで次のアクションに進まない
