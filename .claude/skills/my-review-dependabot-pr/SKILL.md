---
name: my-review-dependabot-pr
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

ただしセキュリティ面（CVE/GHSA・深刻度・影響範囲・修正導入版）に限れば、紐づく Dependabot alert（後述）が最も確実な一次情報で、Release notes より優先する。

ghro コマンド例:

- メタ情報: `ghro pr view <PR番号> --repo owner/repo --json number,title,body,headRefName,labels,additions,deletions,files`
- 差分本体: `ghro pr diff <PR番号> --repo owner/repo`
- CI 状況: `gh pr checks <PR番号> --repo owner/repo`
- Dependabot alert 一覧: `ghro api 'repos/<owner>/<repo>/dependabot/alerts?state=open&per_page=100' --paginate --jq '.[] | {number, package: .dependency.package.name, manifest: .dependency.manifest_path, ghsa: .security_advisory.ghsa_id, cve: .security_advisory.cve_id, severity: .security_advisory.severity, range: .security_vulnerability.vulnerable_version_range, first_patched: .security_vulnerability.first_patched_version.identifier}'`（デフォルトは全 state・1 ページ 30 件で照合漏れするので、open に絞ってページングする）

Dependabot body の読み方:

Dependabot は body に決まったセクションを埋め込むので、該当箇所だけ拾う:

- `Release notes` — 各バージョンのリリースノート。Breaking Changes / セキュリティ修正の主な情報源
- `Commits` — 期間内のコミット一覧。リリースノートに載らない小さな破壊的変更が出ることがある

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
- 間接依存（依存先の依存）への混入
- 乗っ取られたメンテナアカウントから push されたタグ（署名検証なしでは正当性を保証できない）

### Dependabot alert 起因か必ず照合する

Dependabot PR にはバージョンアップ起因とセキュリティ修正起因があり、後者だけがリポジトリの Dependabot alert に紐づく。PR 単体ではどちらか判別しづらく、PR と alert の紐づけは API でも直接は取れないため、alert 一覧との照合で判定する。セキュリティ起因かどうかに関わらず Dependabot PR では毎回実施する:

- 上の「Dependabot alert 一覧」コマンドで alert を取得し、PR と次がすべて一致する alert を探す
  - パッケージ名が一致
  - manifest パスが一致（例: `package-lock.json`）
  - `range` に現行バージョンが含まれ、`first_patched` を PR の新バージョンが満たす（＝この PR で解消される）
  - state が open であること（未解消）は一覧コマンドの `state=open` で保証済み
- 一致する alert があれば、その PR は alert 起因（セキュリティ修正）。見つからなければ通常のバージョンアップ起因と判断する

一致した alert の詳細を引けば、Release notes に載らないこともある CVE/GHSA・深刻度・影響範囲・修正導入版を確実に取得でき、これがセキュリティ判定の一次情報になる:

- `ghro api repos/<owner>/<repo>/dependabot/alerts/<番号> --jq '{number, state, ghsa: .security_advisory.ghsa_id, cve: .security_advisory.cve_id, severity: .security_advisory.severity, summary: .security_advisory.summary, range: .security_vulnerability.vulnerable_version_range, first_patched: .security_vulnerability.first_patched_version.identifier, url: .html_url}'`
- ユーザーから alert の URL/番号を直接渡された場合は、その番号で詳細を引くのが最短

1 つの PR が複数の脆弱性を一度にまとめて修正することもあるので、複数の alert が一致しないか意識する。

## 出力形式

各項目は diff・PR body・Release notes・Dependabot alert を調べ、判断と根拠をあわせて記載する。テンプレートの各フィールドがそのまま調査すべき観点になる。上流 release 差分（補助）で不審点があれば「未確認事項」または「結論」で触れる。

`tmp/docs/pr-review-{PR番号}.md` に以下の構成で出力する:

```markdown
# PR #{PR番号}: {PRタイトル}

## 概要

- パッケージ: {パッケージ名} {旧バージョン} → {新バージョン}
- 依存の種類: {ランタイム依存 / 開発依存}
- バージョン変更: {メジャー / マイナー / パッチ}

## 主要な確認結果

- Breaking Changes: {有無と詳細}
- セキュリティ: {紐づく Dependabot alert と CVE / GHSA・深刻度・影響範囲・修正導入版。この PR で解消されるか。なければ「なし」}
- サポートバージョン: {言語・ランタイムの最低バージョン変更の有無}
- CI 状況: {pass / fail / pending と失敗の概要}
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
- 出力するレポートは PR レビューコメントとして GitHub に投稿する前提で書く。GitHub-flavored Markdown の自動リンクに注意する:
  - 裸の `#123` は Issue/PR へ自動リンクされてしまうので、そのまま書かない
  - Dependabot alert 番号は `[alert #123](https://github.com/<owner>/<repo>/security/dependabot/123)` のように対象 URL への Markdown リンクにする。セキュリティ欄・結論など、言及するたびに毎回リンクにする
