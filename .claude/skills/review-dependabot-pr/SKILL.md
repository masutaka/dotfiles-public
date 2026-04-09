---
name: review-dependabot-pr
description: Dependabot PR をレビューし、tmp/docs/pr-review-{PR番号}.md にまとめる
disable-model-invocation: true
argument-hint: [GitHub PR URL or PR番号]
---

ghro CLI で $0（GitHub PR URL または PR 番号）をレビューして、tmp/docs/pr-review-{PR番号}.md にまとめて。

$0 が指定されていない場合は、ユーザーに GitHub PR URL または PR 番号を確認すること。

## 情報収集

情報ソースの優先順位:

1. PR の diff と manifest / lockfile の変更内容
2. PR の body
3. Release Notes / CHANGELOG（body にリンクがあれば確認）

## レビュー観点

以下の観点で調査し、各項目について判断と根拠を記載する:

- 変更されたパッケージの種類（ランタイム依存 / 開発依存）
- バージョン変更の種類（メジャー / マイナー / パッチ）
- 破壊的変更（Breaking Changes）の有無
- セキュリティ修正かどうか
- 影響範囲（変更されたロックファイルやマニフェストから判断）
- 更新対象以外のパッケージが連鎖的に更新されていないか

不明な点は推測せず「未確認」と記載する。

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
- セキュリティ: {該当する CVE があれば記載。なければ「なし」}
- サポートバージョン: {言語やランタイムの最低バージョン変更の有無}
- 連鎖更新: {更新対象以外の意図しない依存変更の有無}

## 影響範囲

{manifest / lockfile / diff から見える影響の分析}

## 未確認事項

{情報不足や追加確認が必要な点。なければこのセクション自体削除すること}

## 結論

{問題なし / 要確認 / マージ非推奨}

{判断の根拠を簡潔に}
```

## 注意事項

- このスキルはレビューと報告のみを行い、PR のマージや承認は行わない
