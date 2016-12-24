;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For isolation environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if load-file-name
    (setq user-emacs-directory (file-name-directory load-file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst hostname (downcase (car (split-string (system-name) "\\."))))

(defconst here-is-feedforce (equal hostname "masutaka-pro"))

(defconst machine-linux (eq system-type 'gnu/linux) "Linux")
(defconst machine-mac (eq system-type 'darwin) "Mac")

(defconst running-24 (eq emacs-major-version 24) "running emacs 24.xx")

(defconst my-download-directory "~/tmp" "The directory for download.")
(defconst my-elisp-directory (expand-file-name "elisp" user-emacs-directory) "The directory for my elisp file.")

(defconst day-name-alist '(("Sun" . "日") ("Mon" . "月") ("Tue" . "火")
			   ("Wed" . "水") ("Thu" . "木") ("Fri" . "金") ("Sat" . "土")))
(defconst day-name-jp-list (mapcar (lambda (cell) (cdr cell)) day-name-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(package-refresh-contents)

(package-install 'auto-complete)
(package-install 'blgrep)
(package-install 'clmemo)
(package-install 'codic)
(package-install 'coffee-mode)
(package-install 'dockerfile-mode)
(package-install 'egg)
(package-install 'eldoc-extension)
(package-install 'elscreen)
(package-install 'flycheck)
(package-install 'git-dwim)
(package-install 'github-browse-file)
(package-install 'go-autocomplete)
(package-install 'go-eldoc)
(package-install 'go-mode)
(package-install 'haml-mode)
(package-install 'helm)
(package-install 'helm-bundle-show)
(package-install 'helm-descbinds)
(package-install 'helm-ghq)
(package-install 'helm-github-stars)
(package-install 'helm-hatena-bookmark)
(package-install 'helm-qiita)
(package-install 'helm-swoop)
(package-install 'highlight-symbol)
(package-install 'hl-line+)
(package-install 'keyfreq)
(package-install 'lua-mode)
(package-install 'markdown-mode)
(package-install 'mkdown)
(package-install 'navi2ch)
(package-install 'nginx-mode)
(package-install 'open-junk-file)
(package-install 'org-tree-slide)
(package-install 'php-mode)
(package-install 'quickrun)
(package-install 'rspec-mode)
(package-install 'savekill)
(package-install 'scratch-log)
(package-install 'sequential-command)
(package-install 'slim-mode)
(package-install 'terraform-mode)
(package-install 'web-mode)
(package-install 'wgrep)
(package-install 'yaml-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Advices and Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(defun duplicate-thing (n)
  "http://d.hatena.ne.jp/syohex/20120325/1332641491"
  (interactive "p")
  (save-excursion
    (let (start end)
      (cond (mark-active
             (setq start (region-beginning) end (region-end)))
            (t
             (beginning-of-line)
             (setq start (point))
             (forward-line)
             (setq end (point))))
      (kill-ring-save start end)
      (dotimes (i (or n 1))
        (yank)))))

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))
 
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(defadvice find-alternate-file
  (around revival-point activate)
  "ファイルを読み直す時だけ、カーソル位置を保持する。"
  (let* ((filename (ad-get-arg 0))
	 (point (if (string= (expand-file-name filename)
			     (buffer-file-name))
		    (point))))
    ad-do-it (if point (goto-char point))))

(defun copy-this-buffer-file-name ()
  "現在のバッファに対応したファイル名を kill-ring に先頭に詰める。
対応したファイル名がなかったら nil を返す。"
  (interactive)
  (let ((bfn (buffer-file-name)))
    (when (stringp bfn)
      (kill-new bfn)
      (message bfn))))

(defun dec2hex-hex2dec (num)
  "10進数->16進数, 16進数->10進数"
  (interactive (list (read-string "Num?: " (current-word))))
  (let ((case-fold-search t) string base)
    (if (string-match "^0x\\(.+\\)" num)
	(setq string "0x%s is %d."
	      base 16
	      num (match-string 1 num))
      (setq string "%s is 0x%08x."
	    base 10))
    (message string num (string-to-number num base))))

(defun kill-current-line (&optional arg)
  "現在の行を改行ごと killします。"
  (interactive "P")
  (let ((kill-whole-line t))
    (save-excursion
      (beginning-of-line)
      (kill-line arg)))
  (setq this-command 'kill-current-line))	;;; for yank

(defun move-to-window-line-top ()
  "画面の上端に移動する。"
  (interactive)
  (move-to-window-line 0))

(defun move-to-window-line-bottom ()
  "画面の下端に移動する。"
  (interactive)
  (move-to-window-line -1))

(defun my-beginning-of-buffer ()
  "\\[beginning-of-buffer] without markset"
  (interactive)
  (goto-char (point-min)))

(defun my-end-of-buffer ()
  "\\[end-of-buffer] without markset"
  (interactive)
  (goto-char (point-max)))

(defun my-lisp-load (filename)
  "Load lisp from FILENAME"
  (let ((fullname (expand-file-name (concat "spec/" filename) user-emacs-directory))
        lisp)
    (when (file-readable-p fullname)
      (with-temp-buffer
        (progn
          (insert-file-contents fullname)
          (setq lisp 
                (condition-case nil
                    (read (current-buffer))
                  (error ()))))))
    lisp))

(defun my-yank-pop ()
  (interactive)
  (if (minibufferp)
      (yank-pop)
    (helm-show-kill-ring)))

(defun quote-region (beg end &optional name quote)
  "リージョンの行頭に NAME> を挿入します。QUOTE で引用符を指定できます。"
  (interactive "*r\nP")
  (if (> beg end)
      (let (tmp) (setq tmp beg beg end end tmp)))
  (if (null name)
      (setq name (read-string "From: (default nil) ")))
  (if (or (string= quote "") (null quote))
      (setq quote ">"))
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    (let (str)
      (while (< (point) end)
	(setq str
	      (concat
	       name quote
	       (unless (= (point-at-bol) (point-at-eol)) " ")))
	(insert str)
	(forward-line 1)
	(setq end (+ end (length str)))))))

(defun quote-yank (arg)
  "文字列の行頭に 名前> を付加しながら \\[yank] します。
\\[universal-argument] \\[quote-yank] すると引用符を指定できます。"
  (interactive "P")
  (let ((quote (when arg (read-string "str: (default >) ")))
	(name (read-string "From: (default nil) ")))
    (yank)
    (quote-region (region-beginning) (region-end) name quote)))

(defun scroll-down-one-line (num &optional both)
  "1行スクロールダウンします。
bothが non-nilの場合は、両方のWindowがスクロールダウンします。"
  (interactive "p")
  (scroll-up-one-line (- num) both))

(defun scroll-up-one-line (num &optional both)
  "1行スクロールアップします。
bothが non-nilの場合は、両方のWindowがスクロールアップします。"
  (interactive "p")
  (scroll-up num)
  (when both (scroll-other-window num)))

(defun scroll-down-one-line-both-window (num &optional both)
  "2 分割している場合、両方の Window が 1 行スクロールダウンします。"
  (interactive "p")
  (scroll-down-one-line num t))

(defun scroll-up-one-line-both-window (num &optional both)
  "2 分割している場合、両方の Window が 1 行スクロールアップします。"
  (interactive "p")
  (scroll-up-one-line num t))

(defun shell-insert-result (command)
  "shell-commandの結果を prompt&コマンド名付きでカーソル位置に挿入します。"
  (interactive (list (read-shell-command "Shell command: ")))
  (insert "% " command "\n")
  (shell-command command t))

(defun toggle-variable (var)
  "Toggle '(symbol-value var)'."
  (set var (not (symbol-value var)))
  (message "'%s' is now '%s'"
	   (symbol-name var) (symbol-value var)))

(defun toggle-line-move-visual ()
  "Toggle 'line-move-visual'."
  (interactive)
  (toggle-variable 'line-move-visual))

(defun toggle-require-final-newline ()
  "Toggle 'require-final-newline'."
  (interactive)
  (toggle-variable 'require-final-newline))

(defun yomi (arg)
  "kakasi を使用して、カーソル下の漢字の読みがなを返します。
\\[universal-argument] を付けると、chasen を使用します。
\\[universal-argument]\\[universal-argument] を付けると、mecab を使用します。"
  (interactive "P")
  (let ((buf "*yomi*")
	(command)
	(string (read-string "kanji: " (current-word))))
    (setq command
	  (cond
	   ((equal arg '(4))
	    "chasen -j -F '\%y '")
	   ((equal arg '(16))
	    "mecab -O wakati | mecab -O yomi")
	   (t
	    "kakasi -JH")))
    (shell-command (concat "echo \"" string "\" | " command) buf)
    (kill-buffer buf)))

(defun window-toggle-division ()
  "ウィンドウ 2 分割時に、縦分割<->横分割"
  (interactive)
  (unless (= (count-windows 1) 2)
    (error "ウィンドウが 2 分割されていません。"))
  (let ((before-height)
	(other-buf (window-buffer (next-window))))
    (setq before-height (window-height))
    (delete-other-windows)
    (if (= (window-height) before-height)
        (split-window-vertically)
      (split-window-horizontally))
    (other-window 1)
    (switch-to-buffer other-buf)
    (other-window -1)))

(defun swap-buffers ()
  "Swapping buffers in two windows"
  (interactive)
  (let* ((current-w (frame-selected-window))
	 (current-b (window-buffer current-w))
	 (other-w (get-lru-window))
	 (other-b (window-buffer other-w)))
    (unless (one-window-p)
      (select-window current-w)
      (switch-to-buffer other-b)
      (select-window other-w)
      (switch-to-buffer current-b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 指定したディレクトリリスト以下を load-path に追加。
(dolist (dir (let ((dir (expand-file-name my-elisp-directory)))
	       (list dir (format "%s%d" dir emacs-major-version))))
  (when (and (stringp dir) (file-directory-p dir))
    (let ((default-directory dir))
      (add-to-list 'load-path default-directory)
      ;; カレントディレクトリ以下全て load-path に追加する。
      ;; パスを通したくないディレクトリには、.nosearch という
      ;; ファイルを置く。ディレクトリ RCS と CVS は追加しない。
      (normal-top-level-add-subdirs-to-load-path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; set Kanji coding system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://www.sakito.com/2010/05/mac-os-x-normalization.html
(when machine-mac
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; common setup for Window-System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when window-system

  ;; ツールバー非表示
  (tool-bar-mode 0)

  ;; スクロールバー非表示
  (scroll-bar-mode 0)

  ;; カーソルを永久に点滅
  (blink-cursor-mode 1)
  (setq blink-cursor-blinks 0)

  ;; マウスペースト時カーソル位置に挿入
  (setq mouse-yank-at-point t)

  ;; 対応する括弧をハイライト
  (show-paren-mode 1)

  ;; 画像ファイルを表示
  (auto-image-file-mode t)

  ;; http://d.hatena.ne.jp/setoryohei/20110117/1295336454
  (when machine-mac
    ;; フレームのフォントを設定
    (let* ((size 14) ; ASCIIフォントのサイズ [9/10/12/14/15/17/19/20/...]
	   (asciifont "Menlo") ; ASCIIフォント
	   (jpfont "Hiragino Maru Gothic ProN") ; 日本語フォント
	   (applefont "AppleMyungjo")
	   (h (* size 10))
	   (fontspec (font-spec :family asciifont))
	   (jp-fontspec (font-spec :family jpfont))
	   (apple-fontspec (font-spec :family applefont)))
      (set-face-attribute 'default nil :family asciifont :height h)
      (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
      (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
      (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
      (set-fontset-font nil '(#x0370 . #x03FF) fontspec)	; ギリシャ文字 
      (set-fontset-font nil '(#xE000 . #xF8FF) apple-fontspec)	; アップルマークとか
      )
    ;; フォントサイズの比を設定
    (dolist (elt '(("^-apple-hiragino.*" . 1.2)
		   (".*osaka-bold.*" . 1.2)
		   (".*osaka-medium.*" . 1.2)
		   (".*courier-bold-.*-mac-roman" . 1.0)
		   (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
		   (".*monaco-bold-.*-mac-roman" . 0.9)))
      (add-to-list 'face-font-rescale-alist elt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'helm-config)
(require 'helm-imenu)
(require 'helm-ghq)

;; C-c F1 などのインターフェイスを提供。
(helm-descbinds-mode)

;; Add ghq to after buffers-list
(setq helm-for-files-preferred-list (delete 'helm-source-buffers-list helm-for-files-preferred-list))
(add-to-list 'helm-for-files-preferred-list 'helm-source-ghq)
(add-to-list 'helm-for-files-preferred-list 'helm-source-buffers-list)

;; Remove locate
(setq helm-for-files-preferred-list (delete 'helm-source-locate helm-for-files-preferred-list))

(setq helm-hatena-bookmark-username "masutaka26")
(setq helm-hatena-bookmark-debug-mode t)
(helm-hatena-bookmark-initialize)

(setq helm-qiita-username "masutaka")
(setq helm-qiita-organization "feedforce")
(setq helm-qiita-access-token (my-lisp-load "helm-qiita-access-token"))
(setq helm-qiita-debug-mode t)
(helm-qiita-initialize)

(setq helm-github-stars-token (my-lisp-load "helm-github-stars-token"))
(setq helm-github-stars-name-length 50)
(setq helm-github-stars-refetch-time 0.5)

(defun helm-my-bookmark ()
  "Search Hatena:Bookmark and Qiita Stocks using `helm'."
  (interactive)
  (helm :sources '(helm-hatena-bookmark-source helm-qiita-source)
	:prompt "Find Bookmark: "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'auto-complete-config)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(ac-config-default)
(setq ac-ignore-case nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Backup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((target-dir (expand-file-name "~/"))
      (dest-dir (expand-file-name "~/.Trash/")))

  ;; 自動保存ファイル(#ファイル名#)の作成先変更
  (add-to-list 'auto-save-file-name-transforms
               `(,(concat target-dir "\\([^/]*/\\)*\\([^/]*\\)$")
                 ,(concat dest-dir "\\2")
		 t))

  ;; バックアップファイル(ファイル名~)の作成先変更
  (add-to-list 'backup-directory-alist (cons target-dir dest-dir))

  ;; 自動保存リスト(.saves-<PID>-<HOSTNAME>)の作成先変更
  (setq auto-save-list-file-prefix (expand-file-name ".saves-" dest-dir)))

;; バージョン管理されたファイルをセーブする時にコピーを作らない。
(setq vc-make-backup-files nil)

;; `vc-make-backup-files' が無視されてしまう？？？
(defadvice vc-before-save
  (around vc-make-backup-files activate)
  (if vc-make-backup-files ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; major mode for editing C, C++, Objective-C, and Java code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun comment-dwim-like-c-mode (arg)
  (interactive "*P")
  (let ((comment-start "/* ")
	(comment-end " */"))
    (comment-dwim arg)))

(defun c-mode-common-hook-func ()
  (c-set-style "bsd")
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)
  ;; 演算式が複数行にまたがるときのオフセット
  (c-set-offset 'statement-cont 'c-lineup-math)
  ;; 行末のスペースやタブに色づけして警告する。
  (setq show-trailing-whitespace t)
  (define-key c-mode-base-map (kbd "M-e") 'grep)
  (define-key c-mode-base-map (kbd "M-j") 'goto-line)
  (define-key c-mode-base-map (kbd "C-c C-c") nil)
  (define-key c-mode-base-map (kbd "C-c C-o") 'ff-find-other-file)
  (define-key c-mode-base-map (kbd "C-c C-[") 'beginning-of-defun)
  (define-key c-mode-base-map (kbd "C-c C-]") 'end-of-defun))
(add-hook 'c-mode-common-hook 'c-mode-common-hook-func)

(defun c++-mode-hook-func ()
  (define-key c++-mode-map (kbd "M-;") 'comment-dwim-like-c-mode))
(add-hook 'c++-mode-hook 'c++-mode-hook-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ChangeLogメモ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "add-log" (require 'clmemo))
(autoload 'clgrep "clgrep" "grep mode for ChangeLog Memo file" t)

(defadvice add-log-iso8601-time-string-with-weekday
  (around with-japanese-weekday activate)
  "ChangeLogメモに日本語の曜日を入れる。"
  (setq ad-return-value
	(let ((system-time-locale "C"))
	  (concat (add-log-iso8601-time-string)
		  " (" (cdr (assoc (format-time-string "%a") day-name-alist)) ")"))))

(defun mkchalow-ura (force)
  (interactive "P")
  (let (pro
	(pnm "mkchalow-ura")
	(buf " *mkchalow-ura*")
	(cnm "mkchalow-ura")
	(opts (if force '("-f"))))
    (message (format "%sBuilding chalow for %s..."
		     (if force "Force " "") hostname))
    (setq pro (apply 'start-process pnm buf cnm opts))
    (set-process-sentinel
     pro
     `(lambda (process string)
	(message ,(format "%sBuilding chalow for %s...done"
			  (if force "Force " "") hostname))
	(kill-buffer ,buf)))))

(defun open-chalow ()
  "Open the URL that is matched the article under the cursor"
  (interactive)
  (let* ((date-regexp "^\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)")
	 (date)
	 (entry-id 1)
	 (current (point))
	 (filename (file-name-nondirectory (buffer-file-name)))
	 (base-url
	  (cond
	   ((equal filename "clmemo.txt")
	    "masutaka.net/chalow")
	   (t
	    "0.0.0.0:8080/chalow-ura"))))
    (save-excursion
      (setq date (and (re-search-backward date-regexp (point-min) t)
		      (match-string-no-properties 1))))
    (save-excursion
      (or (re-search-forward date-regexp (point-max) t)
	  (goto-char (point-max)))
      (catch 'loop
	(while (re-search-backward "^	\\* " (point-min) t)
	  (if (<= (point) current)
	      (throw 'loop t)
	    (setq entry-id (+ entry-id 1))))))
    (if date
	(browse-url (format "http://%s/%s-%d.html"
			    base-url date entry-id)))))

(setq user-full-name "Takashi Masuda")
(setq user-mail-address "masutaka.net@gmail.com")

(setq clmemo-file-name (expand-file-name "~/clmemo.txt"))

;; For chalow
(setq clmemo-subtitle-char "[")
(setq clmemo-subtitle-punctuation-char '(" [" . "]"))

;; ChangeLogメモに曜日を入れる。
(setq clmemo-time-string-with-weekday t)

;; 行末のスペースやタブに色づけして警告する。
(defun change-log-mode-hook-func ()
  (setq show-trailing-whitespace t))
(add-hook 'change-log-mode-hook 'change-log-mode-hook-func)

(with-eval-after-load "clmemo"
  ;; 日本語の曜日も正しく色付けするように変更。
  (setcar (car clmemo-font-lock-keywords)
	  "^\\sw.........[0-9:+ ]*\\((.)\\)?")
  ;; 日付の判断をきちんとやる。
  (setcar (cadr clmemo-font-lock-keywords)
	  "\\[2[0-9][0-9][0-9]-[01]?[0-9]-[0-3]?[0-9]-?[0-9]*\\]")
  (define-key clmemo-mode-map (kbd "C-i") 'indent-for-tab-command)
  (define-key clmemo-mode-map (kbd "C-c C-u") 'mkchalow-ura)
  (define-key clmemo-mode-map (kbd "C-c C-o") 'open-chalow))

(with-eval-after-load "add-log"
  ;; 複数のタグがあってもきちんと色付けする。
  (setcar (assoc "\\[!?\\([^]\n]+\\)\\]\\(:\\| (\\)" change-log-font-lock-keywords)
	  "\\( *\\[.+\\]\\)+ *:"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dictionary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sdic)
(setq sdic-window-height 20)

(defvar dict-bin "/Users/masutaka/src/hg.pqrs.org/commandline-dictionary-app/src/dict"
  "a path of commandline-dictionary-app")

(defun dictionary-app (word)
  "Display the meaning of word using Dictionary.app."
  (interactive (list (sdic-read-from-minibuffer)))
  (set-buffer (get-buffer-create sdic-buffer-name))
  (or (string= mode-name sdic-mode-name) (sdic-mode))
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert (format "============================================================\n%s\n" word))
  (call-process dict-bin
		nil sdic-buffer-name t word
		"Japanese-English" "Japanese" "Japanese Synonyms")
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (sdic-display-buffer 0))

(defun dictionary-search (word)
  (browse-url
   (concat "dict:///" (url-hexify-string word))))

(defun my-dictionary (arg)
  (interactive "P")
  (cond
   ((equal arg '(16))
    (dictionary-app (sdic-read-from-minibuffer)))
   ((equal arg '(4))
    (sdic-describe-word (sdic-read-from-minibuffer)))
   (t
    (dictionary-search (sdic-read-from-minibuffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diff-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "diff-mode"
  (set-face-foreground 'diff-added-face "blue1")
  (set-face-foreground 'diff-removed-face "red")
  (define-key diff-mode-map (kbd "M-0") 'my-delete-current-window)
  (define-key diff-mode-map (kbd "M-2") 'my-create-window)
  (define-key diff-mode-map (kbd "M-o") 'my-next-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired (directory-browsing commands)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dired-eww-find-file ()
  "diredでポイントのあるファイルをewwで開く。"
  (interactive)
  (let ((file (dired-get-filename)))
    (when (y-or-n-p (format "Open 'eww' %s " (file-name-nondirectory file)))
      (eww-open-file file))))

(setq dired-listing-switches "-al")

;; `C' コマンドでコピーした時、ファイルの更新時刻を変える。
(setq dired-copy-preserve-time nil)

;; Dired で今日変更したファイルを色づけ
(when window-system
  (defface dired-todays-face '((t (:foreground "forest green"))) nil)
  (defvar dired-todays-face 'dired-todays-face)

  (defconst month-name-alist
    '(("1"  . "Jan") ("2"  . "Feb") ("3"  . "Mar") ("4"  . "Apr")
      ("5"  . "May") ("6"  . "Jun") ("7"  . "Jul") ("8"  . "Aug")
      ("9"  . "Sep") ("10" . "Oct") ("11" . "Nov") ("12" . "Dec")))

  (defun dired-today-search (arg)
    "Fontlock search function for dired."
    (search-forward-regexp
     (let ((month-name
	    (cdr (assoc (format-time-string "%b") month-name-alist))))
       (if month-name
	   (format
	    (format-time-string
	     "\\(%Y-%m-%d\\|%b %e\\|%%s %e\\) [0-9]....") month-name)
	 (format-time-string
	  "\\(%Y-%m-%d\\|%b %e\\) [0-9]....")))
     arg t))

  (with-eval-after-load "dired"
    (font-lock-add-keywords
     'dired-mode
     (list '(dired-today-search . dired-todays-face)))))

(defun dired-mode-hook-func ()
  (define-key dired-mode-map (kbd "<") 'my-beginning-of-buffer)
  (define-key dired-mode-map (kbd ">") 'my-end-of-buffer)
  (define-key dired-mode-map (kbd "U") 'dired-unmark-all-marks)
  (define-key dired-mode-map (kbd "i") nil)
  (define-key dired-mode-map (kbd "j") 'scroll-up-one-line)
  (define-key dired-mode-map (kbd "k") 'scroll-down-one-line)
  (define-key dired-mode-map (kbd "r") 'dired-view-file)
  (define-key dired-mode-map (kbd "v") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "w") 'dired-eww-find-file)
  (define-key dired-mode-map (kbd "C-t") 'call-last-kbd-macro)
  (define-key dired-mode-map (kbd "M-{") 'my-prev-window)
  (define-key dired-mode-map (kbd "M-}") 'my-next-window)
  (define-key dired-mode-map (kbd "M-o") 'my-next-window))
(add-hook 'dired-mode-hook 'dired-mode-hook-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'eldoc-extension)

;; 変数を評価した時に出力を省略しない。
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)
(setq eval-expression-debug-on-error nil)

(defun elisp-insert-kbd ()
  "入力したキーを (kbd \"hoge\") に翻訳してカーソル位置に挿入する。"
  (interactive)
  (insert (format "(kbd \"%s\")"
		  (key-description (read-key-sequence "")))))

(defun elisp-mode-common-hook-func ()
  (turn-on-eldoc-mode)
  (local-set-key (kbd "C-c C-b") 'edebug-defun)
  (local-set-key (kbd "C-c C-d") 'eval-defun)
  (local-set-key (kbd "C-c C-[") 'beginning-of-defun)
  (local-set-key (kbd "C-c C-]") 'end-of-defun)
  (local-set-key (kbd "C-c i")   'elisp-insert-kbd))

(defun emacs-lisp-mode-hook-func ()
  (elisp-mode-common-hook-func)
  (checkdoc-minor-mode))
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-hook-func)

(defun lisp-interaction-mode-hook-func ()
  (elisp-mode-common-hook-func))
(add-hook 'lisp-interaction-mode-hook 'lisp-interaction-mode-hook-func)

;; モードラインの "ElDoc" の表示はいらない。
(with-eval-after-load "eldoc"
  (setcar (cdr (assq 'eldoc-mode minor-mode-alist)) ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elscreen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'elscreen)

(elscreen-start)

(defcustom elscreen-navi2ch-mode-to-nickname-alist
  '(("^navi2ch-" . "Navi2ch"))
  "*Alist composed of the pair of mode-name and corresponding screen-name."
  :type '(alist :key-type string :value-type (choice string function))
  :tag "Navi2ch major-mode to screen nickname alist"
  :set (lambda (symbol value)
	 (custom-set-default symbol value)
	 (elscreen-rebuild-mode-to-nickname-alist))
  :group 'navi2ch)
(elscreen-set-mode-to-nickname-alist 'elscreen-navi2ch-mode-to-nickname-alist)

(defalias 'my-create-window 'elscreen-create)
(defalias 'my-delete-current-window 'elscreen-kill)
(defalias 'my-next-window 'elscreen-next)
(defalias 'my-prev-window 'elscreen-previous)

;; 閉じるボタンは右側
(setq elscreen-tab-display-kill-screen 'right)

(define-key elscreen-map (kbd "C-l") 'elscreen-clone)
(define-key elscreen-map (kbd "C-SPC") 'elscreen-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; font-lock-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-font-lock-mode 1)

(cond
 (window-system

  (set-face-foreground 'default "black")
  (set-face-background 'default "#E2DDC3")
  (set-face-background 'cursor "firebrick")
  (set-face-foreground 'font-lock-builtin-face "forest green")
  (set-face-foreground 'font-lock-string-face (if (string= (face-attribute 'default :background) "black") "gray65" "gray35"))
  (set-face-foreground 'font-lock-variable-name-face "DarkGoldenrod")
  (set-face-foreground 'mode-line "black")
  (set-face-background 'mode-line "gold")
  (set-face-foreground 'mode-line-inactive "black")
  (set-face-background 'mode-line-inactive "CornflowerBlue")
  (set-face-background 'show-paren-match-face "gray60")

  (if (string= (face-attribute 'default :background) "black")
      (set-face-background 'fringe "gray40"))

  ;; For M-x cvs-update
  (with-eval-after-load "pcvs-info"
    (set-face-foreground 'cvs-handled-face "gold4"))

  ;; For sh script
  (with-eval-after-load "sh-script"
    (set-face-foreground 'sh-heredoc-face "goldenrod4")
    (set-face-foreground 'sh-quoted-exec "medium orchid")))
 (t
  (set-face-foreground 'minibuffer-prompt "black")
  (set-face-foreground 'font-lock-comment-face "red")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; egg

(require 'egg)

(setq egg-buffer-hide-section-type-on-start nil)
(setq egg-enable-tooltip t)
(setq egg-max-reflogs 0)

(set-face-foreground 'egg-branch "blue")
(set-face-foreground 'egg-branch-mono "blue")
(set-face-foreground 'egg-help-key "blue")
(set-face-foreground 'egg-term "blue")

(define-key egg-status-buffer-mode-map (kbd "j") 'scroll-up-one-line)
(define-key egg-status-buffer-mode-map (kbd "k") 'scroll-down-one-line)
(define-key egg-log-buffer-mode-map (kbd "j") 'scroll-up-one-line)
(define-key egg-log-buffer-mode-map (kbd "k") 'scroll-down-one-line)
(define-key ctl-x-map (kbd "v s") 'egg-status)
(define-key ctl-x-map (kbd "v l") 'egg-log)

;;; git-dwim

(require 'git-dwim)

(define-key ctl-x-map (kbd "v b") 'git-branch-next-action)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Go
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "go-mode"
  (require 'go-autocomplete)

  (defun go-mode-hook-func ()
    (setq tab-width 2)
    (go-eldoc-setup)
    (flycheck-mode 1))
  (add-hook 'go-mode-hook 'go-mode-hook-func)

  (define-key go-mode-map (kbd "M-.") 'godef-jump)
  (define-key go-mode-map (kbd "M-,") 'pop-tag-mark))

(setq gofmt-command "goimports")

(add-hook 'before-save-hook 'gofmt-before-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Grep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq grep-find-command '("ack --nogroup --nocolor -k " . 28))
(setq grep-find-history
      '("find . -type f -name '*' ! -path '*/.git/*' ! -path '*/tmp/*' ! -path '*/node_modules/*' -print0 | xargs -0 grep -nH -e  /dev/null"
	"ack --nogroup --nocolor --ignore-dir=vendor/bundle -k "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hl-line+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 30 秒間何もしないと、カレント行がハイライトする。
(require 'hl-line+)
(setq hl-line-idle-interval 30)
(set-face-background 'hl-line "darkseagreen2")
(toggle-hl-line-when-idle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Info package for Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (setq Info-directory-list nil)
  (dolist (dir `(,@Info-default-directory-list
		 ,(expand-file-name "share/info" user-emacs-directory)))
    (setq dir (expand-file-name dir))
    (if (file-directory-p dir)
	(add-to-list 'Info-directory-list dir))))

(defun Info-mode-hook-func ()
  (define-key Info-mode-map (kbd "j") 'scroll-up-one-line)
  (define-key Info-mode-map (kbd "k") 'scroll-down-one-line))
(add-hook 'Info-mode-hook 'Info-mode-hook-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keisen.el -- provide facility for drawing ruled-line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'keisen-right-move "keisen" nil t)
(autoload 'keisen-left-move "keisen" nil t)
(autoload 'keisen-up-move "keisen" nil t)
(autoload 'keisen-down-move "keisen" nil t)

(define-key global-map (kbd "C-s-<right>") 'keisen-right-move)
(define-key global-map (kbd "C-s-<left>") 'keisen-left-move)
(define-key global-map (kbd "C-s-<up>") 'keisen-up-move)
(define-key global-map (kbd "C-s-<down>") 'keisen-down-move)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ruby, PHP, HTML, CSS, JavaScript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'php-mode)
(require 'web-mode)

(defun php-mode-hook-func ()
  (c-set-style "gnu")
  (setq tab-width 8)
  (setq indent-tabs-mode nil)
  (setq show-trailing-whitespace t)
  (setq c-basic-offset 2)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'statement-cont 'c-lineup-math)
  (flymake-mode 1))
(add-hook 'php-mode-hook 'php-mode-hook-func)

(setq php-search-url "http://jp.php.net/ja/")
(setq php-manual-url "http://jp.php.net/manual/ja/")

(define-key php-mode-map (kbd "C-.") 'forward-word)
(define-key php-mode-map (kbd "C-c C-[") 'beginning-of-defun)
(define-key php-mode-map (kbd "C-c C-]") 'end-of-defun)

(defun web-mode-hook-func ()
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq indent-tabs-mode nil))
(add-hook 'web-mode-hook 'web-mode-hook-func)

(set-face-foreground 'web-mode-html-attr-name-face "Blue4")
(set-face-foreground 'web-mode-symbol-face "Gold4")
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" . web-mode) t)

;; http://d.hatena.ne.jp/sugyan/20100705/1278306885
(defadvice flymake-post-syntax-check
  (before flymake-force-check-was-interrupted activate)
  (setq flymake-check-was-interrupted t))

;; js-mode
(when (executable-find "jsl")

  (require 'flymake)

  (defun flymake-jsl-init ()
    (list "jsl" (list "-process" (flymake-init-create-temp-buffer-copy
				  'flymake-create-temp-inplace))))

  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.js\\'" flymake-jsl-init flymake-simple-cleanup flymake-get-real-file-name))

  (add-to-list 'flymake-err-line-patterns
	       '("^\\(.+\\)(\\([0-9]+\\)): \\(.*warning\\|SyntaxError\\): \\(.*\\)" 1 2 nil 4))

  (defun js-mode-hook-func ()
    (flymake-mode 1)
    (setq indent-tabs-mode nil))
  (add-hook 'js-mode-hook 'js-mode-hook-func))

(setq js-indent-level 2)

(defun coffee-mode-hook-func ()
 (setq coffee-tab-width 2))

(add-hook 'coffee-mode-hook 'coffee-mode-hook-func)

(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))

(setq ruby-insert-encoding-magic-comment nil)

;; work around
;; http://blog.livedoor.jp/ooboofo3/archives/53748087.html
(with-eval-after-load "ruby-mode"
  (defun ruby-electric-brace (arg)
    (interactive "P")
    (insert-char last-command-event 1)
    (ruby-indent-line t)
    (delete-char -1)
    (self-insert-command (prefix-numeric-value arg))))

(defun ruby-mode-hook-func ()
  ;; http://qiita.com/aKenjiKato/items/9ff1a153691e947113bb
  ;; (setq flycheck-checker 'ruby-rubocop)
  ;; (flycheck-mode 1)
  (setq show-trailing-whitespace t))
(add-hook 'ruby-mode-hook 'ruby-mode-hook-func)

(defun sgml-mode-hook-func ()
  (setq indent-tabs-mode nil)
  (setq show-trailing-whitespace t))
(add-hook 'sgml-mode-hook 'sgml-mode-hook-func)

(defun haml-mode-hook-func ()
  (setq show-trailing-whitespace t))
(add-hook 'haml-mode-hook 'haml-mode-hook-func)

(setq auto-mode-alist (cons '("\\.haml\\'" . haml-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mac port patch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://masutaka.net/chalow/2015-01-04-1.html

(defun mac-selected-keyboard-input-source-change-hook-func ()
  ;; 入力モードが英語の時はカーソルの色をfirebrickに、日本語の時はblackにする
  (set-cursor-color (if (string-match "\\.US$" (mac-input-source))
			"firebrick" "black")))

(add-hook 'mac-selected-keyboard-input-source-change-hook
	  'mac-selected-keyboard-input-source-change-hook-func)

;; ミニバッファにカーソルを移動する際、自動的に英語モードにする
(mac-auto-ascii-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mark
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 矩形操作
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; 選択範囲を反転する。
(setq transient-mark-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun replace-naganen-contents ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward "長年記事")
    (insert "\n")
    (delete-region (point) (point-max))
    (let ((date (format-time-string "%m-%d")))
      (insert (format "\n同じ日に書いた過去の記事はありません。
[2014](https://feedforce.qiita.com/search?sort=&q=user%%3Amasutaka+created%%3A2014-%s)
[2015](https://feedforce.qiita.com/search?sort=&q=user%%3Amasutaka+created%%3A2015-%s)\n" date date)))))

(defun my-markdown-preview ()
  "Run `markdown-command' on the current buffer and view output in browser."
  (interactive)
  (let ((output-file (convert-standard-filename
		      (make-temp-file
		       (expand-file-name "my-markdown" temporary-file-directory)
		       nil ".html"))))
    (browse-url (markdown-export output-file))
    (kill-buffer (get-file-buffer output-file))))

(defun octodown ()
  (interactive)
  (shell-command (format "octodown %s" (buffer-file-name))))

(defun qiitadown ()
  (interactive)
  (shell-command (format "qiitadown.sh %s" (buffer-file-name))))

(with-eval-after-load "markdown-mode"
  (define-key markdown-mode-map (kbd "C-c C-c p") 'my-markdown-preview)
  (define-key markdown-mode-map (kbd "C-c C-c C-c") 'octodown)
  (define-key markdown-mode-map (kbd "C-c C-c C-q") 'qiitadown)
  (define-key markdown-mode-map (kbd "C-c C-i C-n") 'replace-naganen-contents)
  (define-key markdown-mode-map (kbd "C-c C-m") 'browse-url-at-point))

(defun markdown-mode-hook-func ()
  (setq indent-tabs-mode nil)
  (setq show-trailing-whitespace t)
  (setq markdown-css-paths (list mkdown-css-file-name))
  (electric-indent-local-mode 0))
(add-hook 'markdown-mode-hook 'markdown-mode-hook-func)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mode-info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and (require 'mi-config nil t) (require 'mi-fontify nil t))
  (defun help-mode-hook-func ()
    (define-key help-map (kbd "F") 'mode-info-describe-function)
    (define-key help-map (kbd "V") 'mode-info-describe-variable))
  (add-hook 'help-mode-hook 'help-mode-hook-func))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; open-junk-file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'open-junk-file)
(setq open-junk-file-format "~/Dropbox/junk/%Y-%m-%d-%H%M%S.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; quickrun.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'quickrun nil t)
  (define-key global-map (kbd "<f8>") 'quickrun))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sequential-command.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sequential-command)

;;; workaround
;;; seq-count() conflicts with seq.el

(defun sequential-command-count ()
  "Returns number of times `this-command' was executed.
It also updates `seq-start-position'."
  (if (eq last-command this-command)
      (incf seq-store-count)
    (setq seq-start-position  (cons (point) (window-start))
          seq-store-count     0)))

(defmacro define-sequential-command (name &rest commands)
  "Define a command whose behavior is changed by sequence of calls of the same command."
  (let ((cmdary (apply 'vector commands)))
    `(defun ,name ()
       ,(concat "Sequential command of "
                (mapconcat
                 (lambda (cmd) (format "`%s'" (symbol-name cmd)))
                 commands " and ")
                ".")
       (interactive)
       (call-interactively
        (aref ,cmdary (mod (sequential-command-count) ,(length cmdary)))))))

;;; definitions

(define-sequential-command my-beginning-of-line
  beginning-of-line back-to-indentation seq-return)

(define-sequential-command my-end-of-line
  end-of-line seq-return)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shell-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 履歴の数
(setq comint-input-ring-size 1024)

;; パスワードを打つ時に見えなくさせる。
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;;(setq comint-input-ring-file-name "~/.zhistory")

(defun shell-mode-hook-func ()
  ;; エスケープシーケンスを理解するようになる。
  (ansi-color-for-comint-mode-on)
  ;; 実行したコマンドをエコーさせない。
  (setq comint-process-echoes t)
  (define-key shell-mode-map (kbd "<down>") 'comint-next-matching-input-from-input)
  (define-key shell-mode-map (kbd "<up>") 'comint-previous-matching-input-from-input)
  (define-key shell-mode-map (kbd "C-c C-i") 'helm-complete-shell-history)
  (define-key shell-mode-map (kbd "M-?") 'help-for-help)
  (define-key shell-mode-map (kbd "M-h") 'backward-delete-word)
  (define-key shell-mode-map (kbd "M-n") 'comint-next-matching-input-from-input)
  (define-key shell-mode-map (kbd "M-p") 'comint-previous-matching-input-from-input))
(add-hook 'shell-mode-hook 'shell-mode-hook-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sql-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq sql-user "root")
(setq sql-server "localhost")
(setq sql-database "")

(defun sql-interactive-mode-hook-func ()
  (toggle-truncate-lines 1))
(add-hook 'sql-interactive-mode-hook 'sql-interactive-mode-hook-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; view-mode --- peruse file or buffer without editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun view-goto-line-last (&optional line)
  (interactive "P")
  (View-goto-line (if (integerp line) line
		    (line-number-at-pos (point-max)))))

;; read-only の時は view-mode にする。→ edebug やり辛いのでコメントアウト
;(setq view-read-only t)

(with-eval-after-load "view"
  (setcar (cdr (assq 'view-mode minor-mode-alist))
	  (list (propertize " View"
			    'face
			    '(:foreground "white" :background "DeepPink1")))))

(defun view-mode-hook-func ()
  (define-key view-mode-map (kbd "G") 'view-goto-line-last)
  (define-key view-mode-map (kbd "j") 'View-scroll-line-forward)
  (define-key view-mode-map (kbd "k") 'View-scroll-line-backward))
(add-hook 'view-mode-hook 'view-mode-hook-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'generic-x)

(delete '("\\.js\\'" . javascript-generic-mode) auto-mode-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 履歴保存
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-recentf-list-prev nil)

(defadvice recentf-save-list
  (around no-message activate)
  "If `recentf-list' and previous recentf-list are equal,
do nothing. And suppress the output from `message' and
`write-file' to minibuffer."
  (unless (equal recentf-list my-recentf-list-prev)
    (cl-letf (((symbol-function 'message) #'format)
	      ((symbol-function 'write-file)
	       ;; write-file() は内部で C で書かれた write_region() を
	       ;; 呼ぶため、上の message() への抑制は効かない。
	       (lambda (file &optional confirm)
		 (let ((str (buffer-string)))
		   (with-temp-file file
		     (insert str))))))
      ad-do-it
      (setq my-recentf-list-prev recentf-list))))

(defadvice recentf-cleanup
  (around no-message activate)
  "suppress the output from `message' to minibuffer"
  (cl-letf (((symbol-function 'message) #'format))
    ad-do-it))

(setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory))
(setq recentf-max-saved-items 5000)
(setq recentf-exclude '(".recentf"))
(setq recentf-auto-cleanup 10)
(run-with-idle-timer 30 t 'recentf-save-list)
(recentf-mode 1)

;; Emacs 終了時に kill-ring を保存する。
(require 'savekill)

;; scratch バッファを次回起動時に復元。ログも記録する。
(require 'scratch-log)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

;; Avoid to write `package-selected-packages` in init.el
(load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

(setq codic-api-token (my-lisp-load "codic-api-token"))

(global-auto-revert-mode 1)

;; for distnoted patch
(setq use-dialog-box nil)

;; コマンドの使用頻度を表示(M-x keyfreq-show)
(setq keyfreq-file (expand-file-name ".keyfreq" user-emacs-directory))
(setq keyfreq-file-lock (expand-file-name ".keyfreq.lock" user-emacs-directory))
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(when (require 'org-tree-slide nil t)
  (setq org-tree-slide-heading-emphasis t)
  (define-key global-map (kbd "<f5>") 'org-tree-slide-mode)
  (define-key global-map (kbd "S-<f5>") 'org-tree-slide-skip-done-toggle))

(defun org-mode-hook-func ()
  (define-key org-mode-map (kbd "C-,") 'backward-word))
(add-hook 'org-mode-hook 'org-mode-hook-func)

;; ファイル名補完時、常に大文字小文字を区別しない。
(setq read-file-name-completion-ignore-case t)

;; レジスタ挿入時にポイントが挿入した文字列の後ろに移動するように
(defadvice insert-register (before put-point-after activate)
  (setq arg (not arg)))

;; C-u C-SPC, C-u C-SPC,... が C-u C-SPC, C-SPC,... で良くなる。
(setq set-mark-command-repeat-pop t)

;; C-v などでページ移動があってもカーソル位置を変化させない。
(setq scroll-preserve-screen-position t)

(setq shell-history-file "~/.zhistory")

;;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; C-x C-c で終了する前に y or n で問い合わせてくれる。
(setq confirm-kill-emacs 'y-or-n-p)

;; 物理行単位の移動(Emacs23 から設定可能。デフォルトは t)
(setq line-move-visual t)

;; emacsclient
(require 'server)
(unless (server-running-p) (server-start))

;; Navigator for 2ch for Emacsen
(autoload 'navi2ch "navi2ch" "Navigator for 2ch for Emacs" t)

;; ファイルローカル変数を使っていいか、いちいち問い合わせない。
(setq safe-local-variable-values
      '((make-backup-files . t)
	(buffer-file-coding-system . euc-jp)
	(clmemo-mode . t)))

;; /bin/sh を使った ssh を使用する。
(setq tramp-default-method "scpx")

;; ediff で frame を新しく作らない。
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; *scratch* バッファのメッセージは表示させない。
(setq initial-scratch-message nil)

;; comment-region で、空行もコメントアウトする。
(setq comment-empty-lines t)

;; リンク先のファイルを開く。(yes)
(setq vc-follow-symlinks t)

;; fringe(両脇の白い帯)に EOF 情報を表示
;;(setq indicate-empty-lines nil)

(progn
  ;; jpeg ファイルかどうかのチェックを甘くする。
  (with-eval-after-load "image"
    (add-to-list 'image-type-header-regexps (cons "\\`\xff\xd8" 'jpeg)))
  ;; 画像を表示する時は標準のキーバインドにする。
  (with-eval-after-load "image-mode"
    (define-key image-mode-map (kbd "C-n") 'next-line)
    (define-key image-mode-map (kbd "C-p") 'previous-line)))

;; 強力な補完機能 (ex. C-x C-f <stdio.h> [RET])
;;(partial-completion-mode t)

;; キーの一覧を表示を C-xC-h から C-x? に変更。
(setq help-char ??)

;; *Messages*バッファの最大行数
(setq message-log-max 256)

;; 非選択 window での中抜きカーソル表示をやめる。
(setq-default cursor-in-non-selected-windows nil)

;; スタートアップメッセージは表示しない。
(setq inhibit-startup-message t)

;; xxx.tar.gzとかも直接開けるようになる。
(auto-compression-mode t)

;; for compile
(setq compile-command "gcc -Wall -Wextra -g test.c")

;; コンパイル時にスクロールさせる。
(setq compilation-scroll-output t)

;; スクロールは１行ずつ
(setq scroll-conservatively 1)

;; C-x 3 でウィンドウを左右に分割した時、折り返す。
(setq truncate-partial-width-windows nil)

;; バッファの最後の行で next-line しても新しい行を作らない。
(setq next-line-add-newlines nil)

;; ファイルの尻尾に改行を入れるかどうか。
;; (t->常に挿入する、nil->常に挿入しない、それ以外->ユーザに問い合わせる。)
(setq require-final-newline t)

;; 警告音のかわりに画面フラッシュ
(setq visible-bell t)

;; dabbrev-expand (M-/) で大文字と小文字の区別をする。
(setq dabbrev-case-fold-search nil)

;; 「、。」のあとに半角スペースを開けなくても、dabbrev-expand (M-/)を有効にする。
(setq dabbrev-abbrev-char-regexp "[-a-zA-Z_]")

;; 折り返すカラム数
;;(setq-default fill-column 70)

;; -nw 起動時は、menu-bar 非表示
(unless window-system
  (menu-bar-mode 0))

;; 行番号の表示
(line-number-mode t)

;; 桁番号の表示
(column-number-mode t)

;; 実行の許可
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; 時間の書式
(setq display-time-string-forms
      '(year "年" month "月" day "日("
	     (cdr (assoc dayname day-name-alist))
	     ") " 24-hours ":" minutes (if mail " Mail" "")))

;; 時間を表示
(display-time)

;; スクリプトファイル保存時に自動で実行許可フラグを立てる。
(with-eval-after-load "ange-ftp"
  (defadvice executable-make-buffer-file-executable-if-script-p
      (around for-ange-ftp activate)
    "ネットワーク先のファイルには実行しないようにする。"
    (unless (string-match (car ange-ftp-name-format) (buffer-file-name))
      ad-do-it)))
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

(defun sh-mode-hook-func ()
  (setq sh-basic-offset 2)
  (setq sh-indentation 2)
  ;; 行末のスペースやタブに色づけして警告する。
  (setq show-trailing-whitespace t))
(add-hook 'sh-mode-hook 'sh-mode-hook-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key Binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(keyboard-translate ?\C-h ?\C-?)  ; translate `C-h' to DEL
(keyboard-translate ?\C-? ?\C-h)  ; translate DEL to `C-h'.

(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

(define-key isearch-mode-map (kbd "C-k") 'isearch-edit-string)
(define-key isearch-mode-map (kbd "DEL") 'isearch-delete-char)

(define-key global-map (kbd "<f3>") 'highlight-symbol-at-point)

(define-key global-map (kbd "<mouse-4>") (lambda () (interactive) (scroll-down 3)))
(define-key global-map (kbd "<mouse-5>") (lambda () (interactive) (scroll-up 3)))
(define-key global-map (kbd "<mouse-6>") 'previous-buffer)
(define-key global-map (kbd "<mouse-7>") 'next-buffer)

(define-key global-map (kbd "<insert>") nil)
(define-key global-map (kbd "<home>") 'my-beginning-of-buffer)
(define-key global-map (kbd "<end>") 'my-end-of-buffer)
(define-key global-map (kbd "<up>") 'previous-line)
(define-key global-map (kbd "<down>") 'next-line)

;; custom of the C-? key
(define-key global-map (kbd "C-a") 'my-beginning-of-line)
;;(define-key global-map (kbd "C-b") 'backward-char)
;;(define-key global-map (kbd "C-c") Prefix Command)
;;(define-key global-map (kbd "C-d") 'delete-char)
(define-key global-map (kbd "C-e") 'my-end-of-line)
;;(define-key global-map (kbd "C-f") 'forward-char)
;;(define-key global-map (kbd "C-g") 'keyboard-quit)
;;(define-key global-map (kbd "C-h") 'backward-delete-char-untabify)
;;(define-key global-map (kbd "C-i") 'indent-for-tab-command)
;;(define-key global-map (kbd "C-j") 'newline-and-indent)
;;(define-key global-map (kbd "C-k") 'kill-line)
;;(define-key global-map (kbd "C-l") 'recenter)
;;(define-key global-map (kbd "C-m") 'newline)
(define-key global-map (kbd "C-n") 'next-line)
;;(define-key global-map (kbd "C-o") 'open-line)
(define-key global-map (kbd "C-p") 'previous-line)
;;(define-key global-map (kbd "C-q") ctl-q-map)
;;(define-key global-map (kbd "C-r") 'isearch-backward)
;;(define-key global-map (kbd "C-s") 'isearch-forward)
(define-key global-map (kbd "C-t") 'repeat)
;;(define-key global-map (kbd "C-u") 'universal-argument)
;;(define-key global-map (kbd "C-v") 'scroll-up)
;;(define-key global-map (kbd "C-w") 'kill-region)
;;(define-key global-map (kbd "C-x") ctl-x-map)
;;(define-key global-map (kbd "C-y") 'yank)
;;(define-key global-map (kbd "C-z") Prefix Command)
(define-key global-map (kbd "C-,") 'backward-word)
(define-key global-map (kbd "C-.") 'forward-word)

;; custom of the Esc-? key
(define-key esc-map (kbd "SPC") 'cycle-spacing)
(define-key esc-map (kbd "+") 'eval-expression)
(define-key esc-map (kbd "<") 'my-beginning-of-buffer)
(define-key esc-map (kbd ">") 'my-end-of-buffer)
(define-key esc-map (kbd "?") 'help)
(define-key esc-map (kbd "0") 'my-delete-current-window)
(define-key esc-map (kbd "2") 'my-create-window)
(define-key esc-map (kbd "O") 'my-prev-window)
(define-key esc-map (kbd "a") 'yomi)
(define-key esc-map (kbd "b") 'backward-word)
(define-key esc-map (kbd "c") 'compile)
;;(define-key esc-map (kbd "d") 'kill-word)
(define-key esc-map (kbd "e") 'grep)
(define-key esc-map (kbd "f") 'forward-word)
;;(define-key esc-map (kbd "g") goto-map)
(define-key esc-map (kbd "h") 'backward-delete-word)
;;(define-key esc-map (kbd "i") 'tab-to-tab-stop)
;;(define-key esc-map (kbd "j") 'indent-new-comment-line)
(define-key esc-map (kbd "k") 'kill-current-line)
(define-key esc-map (kbd "l") nil)	;; downcase-word
;;(define-key esc-map (kbd "m") 'back-to-indentation)
(define-key esc-map (kbd "n") 'forward-paragraph)
(define-key esc-map (kbd "o") 'my-next-window)
(define-key esc-map (kbd "p") 'backward-paragraph)
;;(define-key esc-map (kbd "q") 'fill-paragraph)
(define-key esc-map (kbd "r") 'toggle-read-only)
;;(define-key esc-map (kbd "s") search-map)
;;(define-key esc-map (kbd "t") 'transpose-words)
;;(define-key esc-map (kbd "u") 'upcase-word)
;;(define-key esc-map (kbd "v") 'scroll-down-command)
;;(define-key esc-map (kbd "w") 'kill-ring-save)
(define-key esc-map (kbd "x") 'helm-M-x)
(define-key esc-map (kbd "y") 'my-yank-pop)
;;(define-key esc-map (kbd "z") 'zap-to-char)
(define-key esc-map (kbd "C-y") (lambda () (interactive) (yank-pop -1)))

;; custom of the Super-? key (see term/ns-win.el)
(define-key global-map (kbd "s-{") 'my-prev-window)
(define-key global-map (kbd "s-}") 'my-next-window)
(define-key global-map (kbd "s-0") 'delete-window)
(define-key global-map (kbd "s-1") 'delete-other-windows)
(define-key global-map (kbd "s-2") 'split-window-below)
(define-key global-map (kbd "s-3") 'split-window-right)
(define-key global-map (kbd "s-9") 'delete-other-windows-vertically)
(define-key global-map (kbd "s-a") 'helm-imenu)
(define-key global-map (kbd "s-b") 'helm-my-bookmark)
(define-key global-map (kbd "s-e") 'helm-elscreen)
(define-key global-map (kbd "s-g") 'helm-github-stars)
(define-key global-map (kbd "s-h") (lambda (arg) (interactive "p") (scroll-left arg t)))
(define-key global-map (kbd "s-j") 'scroll-up-one-line)
(define-key global-map (kbd "s-k") 'scroll-down-one-line)
(define-key global-map (kbd "s-l") (lambda (arg) (interactive "p") (scroll-right arg t)))
(define-key global-map (kbd "s-n") nil)
(define-key global-map (kbd "s-o") nil)
(define-key global-map (kbd "s-s") 'helm-swoop)
(define-key global-map (kbd "s-t") 'my-create-window)
(define-key global-map (kbd "s-w") nil)
(define-key global-map (kbd "s-y") 'duplicate-thing)
(define-key global-map (kbd "s-C-j") 'scroll-up-one-line-both-window)
(define-key global-map (kbd "s-C-k") 'scroll-down-one-line-both-window)

;; custom of the ctl-q-map
(defvar ctl-q-map (make-keymap))
(define-key global-map (kbd "C-q") ctl-q-map)
(define-key ctl-q-map (kbd ".") (if (featurep 'mi-config) 'mode-info-find-tag))
(define-key ctl-q-map (kbd "c") 'copy-this-buffer-file-name)
(define-key ctl-q-map (kbd "g b") 'github-browse-file-blame)
(define-key ctl-q-map (kbd "g f") 'github-browse-file)
(define-key ctl-q-map (kbd "C-a") 'text-scale-adjust)
(define-key ctl-q-map (kbd "C-b") 'backward-list)
(define-key ctl-q-map (kbd "C-c") 'clmemo)
(define-key ctl-q-map (kbd "C-d") 'my-dictionary)
(define-key ctl-q-map (kbd "C-e") 'grep-find)
(define-key ctl-q-map (kbd "C-f") 'forward-list)
(define-key ctl-q-map (kbd "C-g") nil)
;;(define-key ctl-q-map (kbd "C-h") 'shell)		;;; => DEL
(define-key ctl-q-map (kbd "C-i") 'window-toggle-division)
(define-key ctl-q-map (kbd "C-j") 'open-junk-file)
(define-key ctl-q-map (kbd "C-k") 'swap-buffers)
(define-key ctl-q-map (kbd "C-l") 'move-to-window-line)
(define-key ctl-q-map (kbd "C-m") 'comment-region)
(define-key ctl-q-map (kbd "C-n") 'move-to-window-line-bottom)
(define-key ctl-q-map (kbd "C-o") (lambda () (interactive) (other-window -1)))
(define-key ctl-q-map (kbd "C-p") 'move-to-window-line-top)
(define-key ctl-q-map (kbd "C-q") 'quoted-insert)
(define-key ctl-q-map (kbd "C-r") 'rename-uniquely)
(define-key ctl-q-map (kbd "C-s") 'toggle-truncate-lines)
(define-key ctl-q-map (kbd "C-t") 'linum-mode)
(define-key ctl-q-map (kbd "C-u") 'sort-lines)
(define-key ctl-q-map (kbd "C-v") 'mark-whole-buffer)
;;(define-key ctl-q-map (kbd "C-w") nil)
(define-key ctl-q-map (kbd "C-x") 'dec2hex-hex2dec)
(define-key ctl-q-map (kbd "C-y") 'quote-yank)
;;(define-key ctl-q-map (kbd "C-z") nil)
(define-key ctl-q-map (kbd "DEL") 'flyspell-region)
(define-key ctl-q-map (kbd "C-SPC") 'comint-dynamic-complete-filename)

;; custom of the ctl-x-map
(define-key ctl-x-map (kbd "9") 'delete-other-windows-vertically)
(define-key ctl-x-map (kbd "M") 'compose-mail)
(define-key ctl-x-map (kbd "f") 'find-file-literally)
(define-key ctl-x-map (kbd "m") mule-keymap)
(define-key ctl-x-map (kbd "y") 'helm-bundle-show)
;;(define-key ctl-x-map (kbd "C-a") DEFAULT-KEY-PREFIX)
(define-key ctl-x-map (kbd "C-b") 'helm-for-files)
;;(define-key ctl-x-map (kbd "C-c") 'save-buffers-kill-terminal)
;;(define-key ctl-x-map (kbd "C-d") 'list-directory)
;;(define-key ctl-x-map (kbd "C-e") 'eval-last-sexp)
;;(define-key ctl-x-map (kbd "C-f") 'find-file)
(define-key ctl-x-map (kbd "C-g") nil)
(define-key ctl-x-map (kbd "C-h") nil)
(define-key ctl-x-map (kbd "C-i") 'indent-region)
;;(define-key ctl-x-map (kbd "C-j") nil)
(define-key ctl-x-map (kbd "C-k") 'kill-buffer)
;;(define-key ctl-x-map (kbd "C-l") 'downcase-region)
;;(define-key ctl-x-map (kbd "C-m") mule-keymap)
(define-key ctl-x-map (kbd "C-n") nil)
(define-key ctl-x-map (kbd "C-o") 'other-window)
;;(define-key ctl-x-map (kbd "C-p") 'mark-page)
(define-key ctl-x-map (kbd "C-q") 'facemenu-set-default)
;;(define-key ctl-x-map (kbd "C-r") 'find-file-read-only)
;;(define-key ctl-x-map (kbd "C-s") 'save-buffer)
;;(define-key ctl-x-map (kbd "C-t") 'transpose-lines)
;;(define-key ctl-x-map (kbd "C-u") 'upcase-region)
;;(define-key ctl-x-map (kbd "C-v") 'find-alternate-file)
;;(define-key ctl-x-map (kbd "C-w") 'write-file)
;;(define-key ctl-x-map (kbd "C-x") 'exchange-point-and-mark)
;;(define-key ctl-x-map (kbd "C-y") nil)
(define-key ctl-x-map (kbd "C-z") nil)

;;; Local Variables:
;;; mode: emacs-lisp
;;; coding: utf-8
;;; tab-width: 8
;;; End:
