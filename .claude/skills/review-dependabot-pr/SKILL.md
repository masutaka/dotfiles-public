---
name: review-dependabot-pr
description: Dependabot PR をレビューし、tmp/docs/pr-review-{PR番号}.md にまとめる
disable-model-invocation: true
argument-hint: [GitHub PR URL or PR番号]
---

ghro CLI で $0（GitHub PR URL または PR 番号）をレビューして、`tmp/docs/pr-review-{PR番号}.md` にまとめて。

$0 が指定されていない場合は、ユーザーに GitHub PR URL または PR 番号を確認すること。

## 準備

リポジトリ特定:

- GitHub PR URL が渡された場合は URL から `owner/repo` をパースし、ghro に `--repo owner/repo` で渡す
- PR 番号だけが渡された場合は cwd の git リポジトリの origin から `owner/repo` を判定する。判定できなければユーザーに確認する

PR タイトル解析:

- Dependabot のタイトルは `bump <パッケージ名> from <旧バージョン> to <新バージョン>` の形式が多い
- 先にパッケージ名・新旧バージョンを取り出しておくと、以降の調査やセクションの当たりを付けるのが早い

## 情報収集

情報源の優先順位（上から順に確実性が高い）:

1. PR の diff と manifest / lockfile の変更内容（実際に何が変わるかの一次情報）
2. PR body に Dependabot が埋め込む各種セクション
3. Release Notes / CHANGELOG（body にリンクがあれば確認）

ghro コマンド例:

- メタ情報: `ghro pr view <PR番号> --repo owner/repo --json number,title,body,headRefName,labels,additions,deletions,files`
- 差分本体: `ghro pr diff <PR番号> --repo owner/repo`
- CI 状況: `ghro pr checks <PR番号> --repo owner/repo`

Dependabot body の読み方:

Dependabot は body に決まったセクションを埋め込むので、該当箇所だけ拾う:

- `Release notes` — 各バージョンのリリースノート。Breaking Changes / セキュリティ修正の主な情報源
- `Commits` — 期間内のコミット一覧。リリースノートに載らない小さな破壊的変更が出ることがある
- `Compatibility` — Dependabot が算出した compatibility score。「他リポジトリで同 PR が CI を通った割合」の指標で、低いほど警戒材料

不明な点は推測せず「未確認」と記載する。

### 上流 release 差分（補助情報）

レジストリ tarball と git タグの中身は一致しないことがあるため、これだけで混入を断定はできない。あくまで「不審な変更の手がかり」を得るための補助。Dependabot PR body の "Updates ..." リンクやパッケージメタ情報から上流リポジトリを特定し、compare を確認する:

- URL 例: `https://github.com/<owner>/<repo>/compare/<旧タグ>...<新タグ>`
- ghro 経由: `ghro api repos/<owner>/<repo>/compare/<旧タグ>...<新タグ>`（commits / files / 差分行数を JSON で取得）

着目点:

- `postinstall` / `preinstall` / `bin` / `scripts` フィールドの追加・変更
- 不審な URL / IP / Base64 / 長文字列の追加
- 変更内容に対して不自然に大きい diff（minify ファイルの差し替えなど）
- 既知メンテナでない author によるコミット

git diff では検出できないものがあるので過信しないこと:

- レジストリ tarball にのみ混ぜられた悪意コード（git タグと publish 内容が一致しない場合）
- 難読化 / 時限発火 / 環境依存で動作するコード
- transitive な依存への混入
- 乗っ取られたメンテナアカウントから push されたタグ（署名検証なしでは正当性を保証できない）

## レビュー観点

以下の観点で調査し、各項目について判断と根拠を記載する:

- 依存の種類: ランタイム依存 / 開発依存
- バージョン変更: メジャー / マイナー / パッチ
- Breaking Changes: Release notes / Commits から有無を判定
- セキュリティ: CVE / GHSA への言及があれば記載
- サポートバージョン: 言語・ランタイムの最低バージョン変更の有無
- CI 状況: `ghro pr checks` の結果。失敗があれば内容を記載
- Compatibility score: Dependabot body の値。低スコアなら結論判定で警戒材料にする
- 影響範囲: アプリケーションコードへの影響を manifest / diff から判断
- 連鎖更新: lockfile に更新対象以外のパッケージ更新が混ざっていないか
- 上流 release 差分（補助）: postinstall / 不審 URL / 異常な diff 規模 / 不明な author の有無

## 出力形式

`tmp/docs/pr-review-{PR番号}.md` に以下の構成で出力する:

```markdown
# PR #{PR番号}: {PRタイトル}

## 概要

- パッケージ: {パッケージ名} {旧バージョン} → {新バージョン}
- 依存の種類: {ランタイム依存 / 開発依存}
- バージョン変更: {メジャー / マイナー / パッチ}

## 主要な確認結果

- Breaking Changes: {有無と詳細}
- セキュリティ: {該当する CVE / GHSA があれば記載。なければ「なし」}
- サポートバージョン: {言語・ランタイムの最低バージョン変更の有無}
- CI 状況: {pass / fail / pending と失敗の概要}
- Compatibility score: {値（取得できなければ「未掲載」）}
- 連鎖更新: {更新対象以外の意図しない依存変更の有無}

## 影響範囲

{manifest / lockfile / diff から見えるアプリケーションコードへの影響}

## 未確認事項

{情報不足や追加確認が必要な点。なければこのセクション自体削除すること}

## 結論

{問題なし / 要確認 / マージ非推奨}

{判断の根拠を簡潔に}
```

## 注意事項

- このスキルはレビューと報告のみを行い、PR のマージや承認は行わない
