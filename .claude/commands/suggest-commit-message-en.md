現在の "git diff HEAD" に対する、簡潔な英語のコミットメッセージを提案して。

(must) Subject line の先頭には、以下のどれかの prefix を追加して。

- `feat`: (new feature for the user, not a new feature for build script)
- `fix`: (bug fix for the user, not a fix to a build script)
- `docs`: (changes to the documentation)
- `style`: (formatting, missing semi colons, etc; no production code change)
- `refactor`: (refactoring production code, eg. renaming a variable)
- `test`: (adding missing tests, refactoring tests; no production code change)
- `chore`: (updating grunt tasks etc; no production code change)

(must) 実行したコマンドがあれば、Body に含めること。

(may) Body はリスト形式で最大でも 5 行程度に留めること。
