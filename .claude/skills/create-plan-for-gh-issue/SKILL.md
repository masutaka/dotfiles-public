---
name: create-plan-for-gh-issue
description: ghro CLI で Issue を調査し、tmp/docs/plan-for-issue-{Issue番号}.md に作業計画を作成する
disable-model-invocation: true
---

ghro CLI で $0（GitHub Issue URL または Issue 番号）を熟読し、作業計画を tmp/docs/plan-for-issue-{Issue番号}.md として作成して。

$0 が指定されていない場合は、ユーザーに GitHub Issue URL または Issue 番号を確認すること。

## 共通

- docs/ や tmp/docs/ に既存の作業計画があれば参考にすること
- 一次情報である公式ドキュメントを確認すること（context7 MCP Server を活用）
- 作業手順を Phase で分割する。Phase が 1 つで済む場合は分割不要
- 各 Phase で検討の余地を残す。コンパクトな作業計画を作成すること

## ソースコードの変更を伴う場合

- Issue に関連する既存コードを調査し、構造を把握すること
- 具体的なコードの記載は必要最低限に留めること
- 各 Phase は atomic（最小単位）な変更で、1 コミットに対応する想定
- 各 Phase は Green で終わるように、作業計画を作成すること
