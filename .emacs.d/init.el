;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst machine-personal-p (string-match "\\(masutaka-air\\|VivoBook\\)" (system-name)) "Is this a personal computer?")
(defconst machine-work-p (not machine-personal-p) "Is this a work computer?")

(defconst os-mac-p (eq system-type 'darwin) "macOS")
(defconst os-linux-p (eq system-type 'gnu/linux) "Linux")

(defconst my-dark-mode-p
  (let ((cmd (cond
	      (os-mac-p "defaults read -g AppleInterfaceStyle")
	      (os-linux-p "xfconf-query -c xsettings -p /Net/ThemeName"))))
    (string-match "dark" (shell-command-to-string cmd)))
  "On macOS and Linux (Xfce4), it automatically detects if it is dark mode or not.")

(defconst my-cursor-color-for-light "black")
(defconst my-cursor-color-for-dark "gray")
(defconst my-cursor-color-for-im-enabled "DarkOrange2")

(defconst my-elisp-directory (expand-file-name "elisp" user-emacs-directory) "The directory for my elisp file.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-install 'async)
(package-install 'auto-complete)
(package-install 'blgrep)
(package-install 'clmemo)
(package-install 'dockerfile-mode)
(package-install 'egg)
(package-install 'flycheck)
(package-install 'flycheck-rust)
(package-install 'github-browse-file)
(package-install 'go-autocomplete)
(package-install 'go-eldoc)
(package-install 'go-mode)
(package-install 'helm)
(package-install 'helm-descbinds)
(package-install 'helm-ghq)
(package-install 'helm-github-stars)
(package-install 'helm-hatena-bookmark)
(package-install 'helm-swoop)
(package-install 'highlight-symbol)
(package-install 'keyfreq)
(package-install 'markdown-mode)
(package-install 'markdown-preview-mode)
(package-install 'mozc)
(package-install 'nginx-mode)
(package-install 'open-junk-file)
(package-install 'package-lint)
(package-install 'php-mode)
(package-install 'racer)
(package-install 'request)
(package-install 'rspec-mode)
(package-install 'rust-mode)
(package-install 'savekill)
(package-install 'scratch-log)
(package-install 'sequential-command)
(package-install 'terraform-mode)
(package-install 'web-mode)
(package-install 'wgrep)
(package-install 'yaml-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Advices and Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)

;; https://stackoverflow.com/questions/23378271/how-do-i-display-ansi-color-codes-in-emacs-for-any-mode
(defun display-ansi-colors ()
  "Display ANSI color codes in current buffer"
  (interactive)
  (require 'ansi-color)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defadvice find-alternate-file
  (around revival-point activate)
  "ファイルを読み直す時だけ、カーソル位置を保持する。"
  (let* ((filename (ad-get-arg 0))
	 (point (if (string= (expand-file-name filename)
			     (buffer-file-name))
		    (point))))
    ad-do-it (if point (goto-char point))))

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
    (unless (file-readable-p fullname)
      (error (format "Cannot read %s" fullname)))
    (with-temp-buffer
      (progn
        (insert-file-contents fullname)
        (setq lisp
              (condition-case nil
                  (read (current-buffer))
                (error ())))))
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

(defun sort-lines-nocase ()
  "大文字小文字を無視した `sort-lines'"
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

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

(defun toggle-window-division ()
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
(when os-mac-p
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

  (cond
   (os-mac-p
    ;; https://setoryohei.hatenadiary.org/entry/20110117/1295336454
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
      (set-fontset-font nil '(#xE000 . #xF8FF) apple-fontspec))	; アップルマークとか
    ;; フォントサイズの比を設定
    (dolist (elt '(("^-apple-hiragino.*" . 1.2)
		   (".*osaka-bold.*" . 1.2)
		   (".*osaka-medium.*" . 1.2)
		   (".*courier-bold-.*-mac-roman" . 1.0)
		   (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
		   (".*monaco-bold-.*-mac-roman" . 0.9)))
      (add-to-list 'face-font-rescale-alist elt)))
   (os-linux-p
    ;; "Options > Set Default Font..." is helpful for knowing font name.
    (let* ((asciifont "Noto Sans Mono CJK JP") ; ASCIIフォント
	   (jpfont "Noto Sans Mono CJK JP")    ; 日本語フォント
	   (height 110)
	   (jp-fontspec (font-spec :family jpfont)))
      (set-face-attribute 'default nil :family asciifont :height height)
      (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
      (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
      (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "helm-autoloads")
(require 'helm-for-files)
(require 'helm-ghq)

(setq helm-buffer-max-length 50)
(setq helm-candidate-number-limit 500)
(setq helm-move-to-line-cycle-in-source nil) ;; C-n や C-p で、次や前の source に進む

;; C-c F1 などのインターフェイスを提供。
(helm-descbinds-mode)

;; Add ghq to after buffers-list
(let* ((work-list helm-for-files-preferred-list)
       (pos (cl-position 'helm-source-buffers-list work-list))
       (front (cl-subseq work-list 0 (+ pos 1)))
       (back (cl-subseq work-list (+ pos 1))))
  (setq helm-for-files-preferred-list (append front (list 'helm-ghq-source) back)))

;; Remove locate
(setq helm-for-files-preferred-list (delete 'helm-source-locate helm-for-files-preferred-list))

;; Remap
(define-key global-map [remap execute-extended-command] 'helm-M-x)

;; 日本語入力 OFF でミニバッファに入り、終わったら元に戻す。
(when os-linux-p
  (defvar my-last-input-method nil)

  (defun my-helm-before-initialize-hook-func ()
    (setq my-last-input-method current-input-method)
    (deactivate-input-method))
  (add-hook 'helm-before-initialize-hook #'my-helm-before-initialize-hook-func)

  (defun my-helm-after-initialize-hook-func ()
    (if my-last-input-method
	(activate-input-method my-last-input-method)
      (deactivate-input-method)))
  (add-hook 'helm-after-initialize-hook #'my-helm-after-initialize-hook-func))

;;; helm-github-stars.el

(require 'helm-github-stars)

(defvar my-helm-github-stars-interval (* 6 60 60)
  "Number of seconds to call `my-helm-github-stars-async-generate-cache-file'.")

(defvar my-helm-github-stars-timer nil
  "Timer object for GitHub Stars caching will be stored here.
DO NOT SET VALUE MANUALLY.")

(setq helm-github-stars-name-length 50)

(defun my-helm-github-stars-async-generate-cache-file ()
  "Generate `helm-github-stars-cache-file' in the child emacs process"
  (if (my-laptop-is-sleeping-p)
      (message "[GH] Skip, as this laptop seems to be sleeping at %s."
	       (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))
    (async-start
     `(lambda ()
	(let ((start-time (current-time)))
	  (require 'package)
	  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
	  (package-initialize)
	  (require 'helm-github-stars)
	  (setq helm-github-stars-token ,(my-lisp-load "helm-github-stars-token"))
	  (hgs/generate-cache-file)
	  start-time))
     (lambda (start-time)
       (let ((now (current-time)))
	 (message "[GH] Success to GET my GitHub Stars and Repos (%0.1fsec) at %s."
		  (time-to-seconds (time-subtract now start-time))
		  (format-time-string "%Y-%m-%d %H:%M:%S" now)))))))

(defun my-laptop-is-sleeping-p ()
  "Is this laptop sleeping?"
  (interactive)
  (if os-mac-p
      (string-match-p "Yes" (shell-command-to-string "ioreg -r -k AppleClamshellState -d 4 | grep AppleClamshellState | grep Yes"))
    t))

(defun my-helm-github-stars-set-timer ()
  "Set helm-github-stars timer."
  (setq my-helm-github-stars-timer
	(run-at-time "0 sec"
		     my-helm-github-stars-interval
		     #'my-helm-github-stars-async-generate-cache-file)))

(defun my-helm-github-stars-cancel-timer ()
  "Cancel helm-github-stars timer."
  (when my-helm-github-stars-timer
    (cancel-timer my-helm-github-stars-timer)
    (setq my-helm-github-stars-timer nil)))

;; [原因不明] helm-github-stars.el がバイトコンパイルされていると、
;; helm-github-stars-token が nil 判定され、*-by-token 関数を使ってくれないことがある。
(my-helm-github-stars-set-timer)

;;; helm-hatena-bookmark.el

(setq helm-hatena-bookmark-username "masutaka26")
(setq helm-hatena-bookmark-interval (* 3 60 60))
(setq helm-hatena-bookmark-debug-mode t)
(helm-hatena-bookmark-initialize)

;;; My bookmark

(defun my-helm-bookmark ()
  "Search Hatena:Bookmark and Qiita Stocks using `helm'."
  (interactive)
  (helm :sources '(helm-hatena-bookmark-source
		   hgs/helm-c-source-stars
		   hgs/helm-c-source-repos)
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
      (dest-dir (expand-file-name "tmp" user-emacs-directory)))

  (unless (file-exists-p dest-dir)
    (make-directory dest-dir))

  ;; 自動保存ファイル(#ファイル名#)の作成先変更
  (add-to-list 'auto-save-file-name-transforms
               `(,(concat target-dir "\\([^/]*/\\)*\\([^/]*\\)$")
                 ,(concat dest-dir "/\\2")
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
  (define-key c-mode-base-map (kbd "M-j") 'goto-line)
  (define-key c-mode-base-map (kbd "C-c C-c") nil)
  (define-key c-mode-base-map (kbd "C-c C-o") 'ff-find-other-file)
  (define-key c-mode-base-map (kbd "C-c C-[") 'beginning-of-defun)
  (define-key c-mode-base-map (kbd "C-c C-]") 'end-of-defun))
(add-hook 'c-mode-common-hook #'c-mode-common-hook-func)

(defun c++-mode-hook-func ()
  (define-key c++-mode-map (kbd "M-;") 'comment-dwim-like-c-mode))
(add-hook 'c++-mode-hook #'c++-mode-hook-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ChangeLogメモ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Workaround
(if (>= emacs-major-version 26)
    (defvar clmemo-font-lock-keywords
      '(;;
	;; Date lines, with weekday
	("^\\sw.........[0-9:+ ]*\\((...)\\)?"
	 (0 'change-log-date)
	 ("\\([^<(]+?\\)[ \t]*[(<]\\([A-Za-z0-9_.-]+@[A-Za-z0-9_.-]+\\)[>)]" nil nil
	  (1 'change-log-name)
	  (2 'change-log-email)))
	;;
	;; Date
	("\\[[0-9-]+\\]" (0 'clmemo-inline-date-face)))
      "Additional expressions to highlight in ChangeLog Memo mode."))

(with-eval-after-load "add-log" (require 'clmemo))
(autoload 'clgrep "clgrep" "grep mode for ChangeLog Memo file" t)

(defadvice add-log-iso8601-time-string-with-weekday
  (around with-japanese-weekday activate)
  "ChangeLogメモに日本語の曜日を入れる。"
  (setq ad-return-value
	(let ((system-time-locale "ja_JP.UTF-8"))
	  (concat (add-log-iso8601-time-string)
		  " (" (format-time-string "%a") ")"))))

(defun mkchalow-ura (force)
  (interactive "P")
  (let (pro
	(hostname (downcase (car (split-string (system-name) "\\."))))
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
  (let* ((date-regexp "^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)")
	 (date)
	 (entry-id 1)
	 (current (point))
	 (filename (file-name-nondirectory (buffer-file-name)))
	 (scheme
	  (if (equal filename "clmemo.txt") "https" "http"))
	 (base-url
	  (if (equal filename "clmemo.txt") "masutaka.net/chalow" "localhost:8080/chalow-ura")))
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
	(browse-url (format "%s://%s/%s-%d.html"
			    scheme base-url date entry-id)))))

(setq user-full-name "Takashi Masuda")
(setq user-mail-address (my-lisp-load "user-mail-address"))

(setq clmemo-file-name (my-lisp-load "clmemo-file-name"))

;; For chalow
(setq clmemo-subtitle-char "[")
(setq clmemo-subtitle-punctuation-char '(" [" . "]"))

;; ChangeLogメモに曜日を入れる。
(setq clmemo-time-string-with-weekday t)

;; 行末のスペースやタブに色づけして警告する。
(defun change-log-mode-hook-func ()
  (setq show-trailing-whitespace t))
(add-hook 'change-log-mode-hook #'change-log-mode-hook-func)

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

;; Workaround. See https://suzuki.tdiary.net/20161226.html
(if (>= emacs-major-version 26)
    (setq default-fill-column (default-value 'fill-column)))

(require 'sdic)
(setq sdic-window-height 20)

(defun dictionary-search (word)
  (browse-url
   (concat "dict:///" (url-hexify-string word))))

(defun my-dictionary (arg)
  (interactive "P")
  (cond
   ((equal arg '(4))
    (sdic-describe-word (sdic-read-from-minibuffer)))
   (t
    (dictionary-search (sdic-read-from-minibuffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diff-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "diff-mode"
  (define-key diff-mode-map (kbd "M-0") 'tab-close)
  (define-key diff-mode-map (kbd "M-2") 'tab-new)
  (define-key diff-mode-map (kbd "M-o") 'tab-next))

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

;; for ! (dired-do-shell-command)
(if os-mac-p
    (setq dired-guess-shell-alist-user
	  '(("\\.tif\\'" "open")
	    ("\\.pdf\\'" "open")
	    ("\\.png\\'" "open")
	    ("\\.jpe?g\\'" "open"))))

;; Dired で今日変更したファイルを色づけ
(when window-system
  (defface dired-todays-face '((((background light)) (:foreground "forest green"))
			       (((background dark)) (:foreground "green yellow"))) nil)
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
  (define-key dired-mode-map (kbd "M-{") 'tab-previous)
  (define-key dired-mode-map (kbd "M-}") 'tab-next)
  (define-key dired-mode-map (kbd "M-o") 'tab-next))
(add-hook 'dired-mode-hook #'dired-mode-hook-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ediff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; frame を新しく作らない。
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; dark 用の face が見づらいので自分で設定する。
(face-spec-set 'ediff-even-diff-A '((((background dark)) (:background "black" :extend t))))
(face-spec-set 'ediff-even-diff-B '((((background dark)) (:background "dim gray" :extend t))))
(face-spec-set 'ediff-odd-diff-A '((((background dark)) (:background "dim gray" :extend t))))
(face-spec-set 'ediff-odd-diff-B '((((background dark)) (:background "black" :extend t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'eldoc-extension)

;; 変数を評価した時に出力を省略しない。
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)
(setq eval-expression-debug-on-error nil)
(setq edebug-print-length nil)
(setq edebug-print-level nil)

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
(add-hook 'emacs-lisp-mode-hook #'emacs-lisp-mode-hook-func)

(defun lisp-interaction-mode-hook-func ()
  (elisp-mode-common-hook-func))
(add-hook 'lisp-interaction-mode-hook #'lisp-interaction-mode-hook-func)

;; モードラインの "ElDoc" の表示はいらない。
(with-eval-after-load "eldoc"
  (setcar (cdr (assq 'eldoc-mode minor-mode-alist)) ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Face
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when window-system
  (let ((foreground (if my-dark-mode-p "#E1E1E0" "black"))
	(background (if my-dark-mode-p "#2D3743" "#E2DDC3")))
    (face-spec-set 'default `((t :foreground ,foreground :background ,background))))
  (face-spec-set 'cursor `((((background light)) (:background ,my-cursor-color-for-light)) (((background dark)) (:background ,my-cursor-color-for-dark))))
  (face-spec-set 'mode-line '((((background light)) (:background "gold")) (((background dark)) (:background "orange"))))
  (face-spec-set 'region '((((background light)) (:background "lightGoldenrod2")) (((background dark)) (:background "goldenrod4"))))
  (face-spec-set 'font-lock-string-face '((((background light)) (:foreground "gray35")) (((background dark)) (:foreground "gray65"))))
  (face-spec-set 'sh-heredoc '((((background light)) (:foreground "goldenrod4"))))
  (face-spec-set 'sh-quoted-exec '((((background light)) (:foreground "medium orchid")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; egg

(require 'egg)

(setq egg-buffer-hide-section-type-on-start nil)
(setq egg-enable-tooltip t)
(setq egg-max-reflogs 0)

(face-spec-set 'egg-branch '((((background light)) (:foreground "blue")) (((background dark)) (:foreground "yellow3"))))
(face-spec-set 'egg-branch-mono '((((background light)) (:foreground "blue")) (((background dark)) (:foreground "yellow3"))))
(face-spec-set 'egg-diff-add '((((background dark)) (:foreground "light green"))))
(face-spec-set 'egg-help-key '((((background light)) (:foreground "blue")) (((background dark)) (:foreground "yellow3"))))
(face-spec-set 'egg-term '((((background light)) (:foreground "blue")) (((background dark)) (:foreground "yellow3"))))

(define-key egg-status-buffer-mode-map (kbd "j") 'scroll-up-one-line)
(define-key egg-status-buffer-mode-map (kbd "k") 'scroll-down-one-line)
(define-key egg-log-buffer-mode-map (kbd "j") 'scroll-up-one-line)
(define-key egg-log-buffer-mode-map (kbd "k") 'scroll-down-one-line)
(define-key ctl-x-map (kbd "v s") 'egg-status)
(define-key ctl-x-map (kbd "v l") 'egg-log)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Go
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "go-mode"
  (require 'go-autocomplete)

  (defun go-mode-hook-func ()
    (setq tab-width 2)
    (go-eldoc-setup)
    (flycheck-mode 1))
  (add-hook 'go-mode-hook #'go-mode-hook-func)

  (define-key go-mode-map (kbd "M-.") 'godef-jump)
  (define-key go-mode-map (kbd "M-,") 'pop-tag-mark))

(setq gofmt-command "goimports")

(add-hook 'before-save-hook #'gofmt-before-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Grep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq next-error-highlight 3)
(setq next-error-highlight-no-select 3)

(setq grep-find-command '("ack --nogroup --nocolor -kQ ''" . 30))
(setq grep-find-history
      '("LANG=ja_JP.sjis grep -n \"$(echo '検索文字列' | nkf -s)\" * | nkf -w"
	"find . -type f -name '検索文字列' ! -path '*/.git/*' ! -path '*/tmp/*' ! -path '*/node_modules/*' -print0 | xargs -0 grep -nH -e  /dev/null"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hl-line+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 30 秒間何もしないと、カレント行がハイライトする。
(setq hl-line-idle-interval 30)
(require 'hl-line+)
(hl-line-toggle-when-idle 1)
(face-spec-set 'hl-line '((((background light)) (:background "darkseagreen2")) (((background dark)) (:background "RoyalBlue4"))))

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
(add-hook 'Info-mode-hook #'Info-mode-hook-func)

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
;;; Mozc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when os-linux-p
  (require 'mozc)
  (setq default-input-method "japanese-mozc")

  (defun input-method-activate-hook-func ()
    (set-face-background 'cursor my-cursor-color-for-im-enabled))
  (add-hook 'input-method-activate-hook #'input-method-activate-hook-func)

  (defun input-method-deactivate-hook-func ()
    (face-spec-set 'cursor `((((background light)) (:background ,my-cursor-color-for-light))
			     (((background dark)) (:background ,my-cursor-color-for-dark)))))
  (add-hook 'input-method-deactivate-hook #'input-method-deactivate-hook-func)

  (define-key global-map (kbd "s-SPC") 'toggle-input-method)
  (define-key mozc-mode-map (kbd "s-SPC") 'toggle-input-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ruby, PHP, HTML, CSS
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
(add-hook 'php-mode-hook #'php-mode-hook-func)

(setq php-search-url "https://www.php.net/search.php")
(setq php-manual-url "https://www.php.net/manual/ja/")

(define-key php-mode-map (kbd "C-.") 'forward-word)
(define-key php-mode-map (kbd "C-c C-[") 'beginning-of-defun)
(define-key php-mode-map (kbd "C-c C-]") 'end-of-defun)

(defun web-mode-hook-func ()
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq indent-tabs-mode nil))
(add-hook 'web-mode-hook #'web-mode-hook-func)

(face-spec-set 'web-mode-html-attr-name-face '((((background light)) (:foreground "Blue4"))))
(face-spec-set 'web-mode-symbol-face '((((background light)) (:foreground "Gold4"))))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[jg]s\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts[x]?\\'" . web-mode))

;; https://memo.sugyan.com/entry/20100705/1278306885
(defadvice flymake-post-syntax-check
  (before flymake-force-check-was-interrupted activate)
  (setq flymake-check-was-interrupted t))

(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))

(setq ruby-insert-encoding-magic-comment nil)

;; workaround
;; http://blog.livedoor.jp/ooboofo3/archives/53748087.html
(with-eval-after-load "ruby-mode"
  (defun ruby-electric-brace (arg)
    (interactive "P")
    (insert-char last-command-event 1)
    (ruby-indent-line t)
    (delete-char -1)
    (self-insert-command (prefix-numeric-value arg))))

(defun ruby-mode-hook-func ()
  ;; https://qiita.com/katoken-0215/items/9ff1a153691e947113bb
  ;; (setq flycheck-checker 'ruby-rubocop)
  ;; (flycheck-mode 1)
  (setq show-trailing-whitespace t))
(add-hook 'ruby-mode-hook #'ruby-mode-hook-func)

(defun sgml-mode-hook-func ()
  (setq indent-tabs-mode nil)
  (setq show-trailing-whitespace t))
(add-hook 'sgml-mode-hook #'sgml-mode-hook-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'rust-mode (flycheck-rust-setup))

(setq rust-format-on-save t)

(defun rust-mode-hook-func ()
  ;;(eldoc-mode)
  (flycheck-rust-setup)
  (flycheck-mode 1)
  ;;(racer-mode)
  )
(add-hook 'rust-mode-hook #'rust-mode-hook-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mac port patch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; https://masutaka.net/chalow/2015-01-04-1.html

(when os-mac-p
  (defun mac-input-source-ascii-p ()
    (string-match "\\.\\(ABC\\|US\\)$" (mac-input-source)))

  (defun mac-selected-keyboard-input-source-change-hook-func ()
    ;; 入力モードに合わせてカーソル色を切り替える。
    (set-cursor-color (if (mac-input-source-ascii-p)
			  (if my-dark-mode-p my-cursor-color-for-dark my-cursor-color-for-light)
			my-cursor-color-for-im-enabled)))

  (add-hook 'mac-selected-keyboard-input-source-change-hook
	    #'mac-selected-keyboard-input-source-change-hook-func)

  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)

  ;; ミニバッファにカーソルを移動する際、自動的に英語モードにする
  (mac-auto-ascii-mode 1)

  ;; "Emacs 25.1 を EMP版で快適に使う"
  ;; https://qiita.com/takaxp/items/a86ee2aacb27c7c3a902
  ;;
  ;; mac-auto-ascii-mode が Enable かつ日本語入力 ON の時、
  ;; M-x や C-x C-f 等の後に日本語入力 OFF になる問題を救う。

  (defvar mac-win-last-ime-status 'off) ;; {'off|'on}

  (defconst mac-win-kana-input-method "com.google.inputmethod.Japanese.base")

  (defun advice:mac-auto-ascii-setup-input-source (&optional _prompt)
    "Extension to store IME status"
    (mac-win-save-last-ime-status))

  (advice-add 'mac-auto-ascii-setup-input-source :before
              #'advice:mac-auto-ascii-setup-input-source)

  (defun mac-win-save-last-ime-status ()
    (setq mac-win-last-ime-status
          (if (mac-input-source-ascii-p)
              'off 'on)))

  (defun mac-win-restore-ime ()
    (if (mac-win-need-restore-ime)
	(mac-select-input-source mac-win-kana-input-method)))

  (defun mac-win-need-restore-ime ()
    (and mac-auto-ascii-mode (eq mac-win-last-ime-status 'on)))

  ;; M-x 等でミニバッファから元のバッファに戻った後に、日本語入力状態を
  ;; リストアする。
  (add-hook 'minibuffer-setup-hook #'mac-win-save-last-ime-status)
  (add-hook 'minibuffer-exit-hook #'mac-win-restore-ime)

  (defvar mac-win-target-commands
    '(find-file save-buffer other-window split-window delete-window
		delete-other-windows clmemo helm-for-files))

  (defun mac-win-restore-ime-target-commands ()
    (if (and (mac-win-need-restore-ime)
	     (mac-win-target-commands-match))
	(mac-select-input-source mac-win-kana-input-method)))

  (defun mac-win-target-commands-match ()
    (remove-if-not
     (lambda (c)
       (string-match (format "^%s" c) (format "%s" this-command)))
     mac-win-target-commands))

  ;; `mac-win-target-commands' と前方一致する関数の終了後に、日本語入力
  ;; 状態をリストアする
  (add-hook 'pre-command-hook #'mac-win-restore-ime-target-commands))

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

(let ((stylesheet "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.2.0/github-markdown-dark.css"))
  (setq markdown-css-paths (list stylesheet))
  (setq markdown-preview-stylesheets (list stylesheet)))

(defun open-markdown (arg)
  "Open the URL of the article matching the current markdown"
  (interactive "P")
  (if (string-match "github\\.com/route06/docs/\\(.+\\)\\.md$" buffer-file-name)
      (open-github-pages)
    (open-hugo arg)))

(defun open-github-pages ()
  "Open the URL of the GitHub Pages article matching the current markdown"
  (let* ((elements (split-string buffer-file-name "/"))
	 (last-element (car (last elements)))
	 (second-last-element (car (last elements 2)))
	 (trimed-bfn (if (equal (concat second-last-element ".md") last-element)
			 ;; 同じ文字列が連続する場合は後ろの方を削除 /foo/foo.md -> /foo
			 (mapconcat 'identity (butlast elements 1) "/")
		       ;; .md は常に削除 /foo/bar.md -> /foo/bar
		       ;; 特定のファイル名の場合はさらに削除 /foo/index.md -> /foo
		       (replace-regexp-in-string "\\(/index\\|/README\\)?\\.md$" "" buffer-file-name)))
	 (match-string (if (string-match "github\\.com/route06/docs/\\(.+\\)$" trimed-bfn)
			  (match-string-no-properties 1 trimed-bfn))))
    (if match-string
	(browse-url (format "https://docs.route06.co.jp/%s/" match-string)))))

(defun open-hugo (arg)
  "Open the URL of the Hugo article matching the current markdown"
  (let* ((post-regexp "content/posts/\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]+\\)\\.md$")
	 (stock-regexp "content/\\([^/]+\\)\\.md$")
	 (match-string (if (or (string-match post-regexp buffer-file-name)
			       (string-match stock-regexp buffer-file-name))
			   (match-string-no-properties 1 buffer-file-name))))
    (if match-string
	(browse-url (format
		     (if arg "http://localhost:1313/%s/" "https://masutaka.net/%s/")
		     match-string)))))

(with-eval-after-load "markdown-mode"
  (define-key markdown-mode-command-map (kbd "C-p") 'markdown-preview-mode)
  (define-key markdown-mode-map (kbd "C-c C-m") 'browse-url-at-point)
  (define-key markdown-mode-map (kbd "C-c C-o") 'open-markdown))

(defun markdown-mode-hook-func ()
  (setq indent-tabs-mode nil)
  (setq show-trailing-whitespace t)
  (electric-indent-local-mode 0))
(add-hook 'markdown-mode-hook #'markdown-mode-hook-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; occur
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-occur-return ()
  (switch-to-buffer-other-window "*Occur*"))

;;(add-hook 'occur-hook #'my-occur-return)
;;(add-hook 'occur-mode-find-occurrence-hook #'my-occur-return)

(define-key occur-mode-map (kbd "n") 'occur-next-error)
(define-key occur-mode-map (kbd "p") (lambda () (interactive) (occur-next-error -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; open-junk-file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'open-junk-file)
(setq open-junk-file-format (my-lisp-load "open-junk-file-format"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sequential-command.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sequential-command)

(define-sequential-command my-beginning-of-line
  beginning-of-line back-to-indentation seq-return)

(define-sequential-command my-end-of-line
  end-of-line seq-return)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sql-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq sql-user "root")
(setq sql-server "localhost")
(setq sql-database "")

(defun sql-interactive-mode-hook-func ()
  (toggle-truncate-lines 1))
(add-hook 'sql-interactive-mode-hook #'sql-interactive-mode-hook-func)

(defun sql-mode-hook-func ()
  (setq show-trailing-whitespace t))
(add-hook 'sql-mode-hook #'sql-mode-hook-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tab-bar.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tab-bar-mode 1)
(tab-bar-history-mode 1)

(defun my-tab-clone (&optional arg)
  (interactive "P")
  (let ((tab-bar-new-tab-choice t))
    (tab-new arg)))

(defun my-tab-select ()
  "Jump to any tab interactively. The purpose is to jump to tab number 10 or higher."
  (interactive)
  (tab-select
   (string-to-number (read-from-minibuffer "tab number: " "10"))))

(defun my-tab-select-last ()
  "Switch to the last tab."
  (interactive)
  (tab-select (length (funcall tab-bar-tabs-function))))

(setq tab-bar-new-tab-choice "*scratch*")
(setq tab-bar-new-tab-to 'rightmost)
(setq tab-bar-tab-hints t)
(setq tab-bar-tab-name-function #'tab-bar-tab-name-truncated)

;; 現在のタブを見やすくする
(face-spec-set 'tab-bar-tab '((((background light)) (:background "gold")) (((background dark)) (:background "orange"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; terraform-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: import block と moved block にも対応させる。
;; https://developer.hashicorp.com/terraform/language/import
;; https://developer.hashicorp.com/terraform/language/modules/develop/refactoring
(defun open-terraform-document ()
  "Open the document of terraform under the cursor"
  (interactive)
  (let ((regexp "^\\(data\\|resource\\) \"\\([a-z]+\\)_\\([^\"]+\\)\"")
	(type) (type-for-url) (owner) (provider) (name))
    (save-excursion
      (re-search-backward "^[a-z]" (point-min) t)
      (when (re-search-forward regexp (point-at-eol) t)
	(setq type (match-string-no-properties 1)
	      provider (match-string-no-properties 2)
	      name (match-string-no-properties 3))))
    (setq owner
	  (if (equal provider "cloudflare") "cloudflare" "hashicorp")
	  type-for-url
	  (cond
	   ((equal type "resource") "resources")
	   ((equal type "data") "data-sources")))
    (if (and type-for-url provider name)
	(browse-url (format "https://registry.terraform.io/providers/%s/%s/latest/docs/%s/%s"
			    owner provider type-for-url name))
      (message "Unknown terraform DSL"))))

(with-eval-after-load "terraform-mode"
  (set-face-foreground 'terraform--resource-name-face "deep pink")
  (define-key terraform-mode-map (kbd "C-c C-o") 'open-terraform-document))

(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

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
  ;; モードラインの " View" を目立たせる。
  (setcar (cdr (assq 'view-mode minor-mode-alist))
	  (list (propertize " View" 'face '(:foreground "white" :background "DeepPink1")))))

(defun view-mode-hook-func ()
  (define-key view-mode-map (kbd "G") 'view-goto-line-last)
  (define-key view-mode-map (kbd "j") 'View-scroll-line-forward)
  (define-key view-mode-map (kbd "k") 'View-scroll-line-backward))
(add-hook 'view-mode-hook #'view-mode-hook-func)

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

;; 汎用 major-mode
(require 'generic-x)

;; M-x align-regexp で Tab の代わりに Space を使う
(defadvice align-regexp
    (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

;; Looker
(add-to-list 'auto-mode-alist '("\\.lkml\\'" . default-generic-mode))

;; Avoid to write `package-selected-packages` in init.el
(load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

(global-auto-revert-mode 1)

;; Instead of the global-auto-revert-mode
(setq revert-without-query '(".+"))

;; for distnoted patch
(setq use-dialog-box nil)

;; コマンドの使用頻度を表示(M-x keyfreq-show)
(setq keyfreq-file (expand-file-name ".keyfreq" user-emacs-directory))
(setq keyfreq-file-lock (expand-file-name ".keyfreq.lock" user-emacs-directory))
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

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

;;(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; C-x C-c で終了する前に y or n で問い合わせてくれる。
(setq confirm-kill-emacs 'y-or-n-p)

;; 物理行単位の移動(Emacs23 から設定可能。デフォルトは t)
(setq line-move-visual t)

;; emacsclient
(require 'server)
(unless (server-running-p) (server-start))

;; /bin/sh を使った ssh を使用する。
(setq tramp-default-method "scpx")

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
(setq message-log-max 2000)

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
(setq-default fill-column 120)

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
      '((format-time-string "%Y/%m/%d (%a) " now)  24-hours ":" minutes (if mail " Mail" "")))

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
	  #'executable-make-buffer-file-executable-if-script-p)

(defun sh-mode-hook-func ()
  (setq sh-basic-offset 2)
  (setq sh-indentation 2)
  ;; 行末のスペースやタブに色づけして警告する。
  (setq show-trailing-whitespace t))
(add-hook 'sh-mode-hook #'sh-mode-hook-func)

(defun python-mode-hook-func ()
  ;; 行末のスペースやタブに色づけして警告する。
  (setq show-trailing-whitespace t))
(add-hook 'python-mode-hook #'python-mode-hook-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key Binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(keyboard-translate ?\C-h ?\C-?)  ; translate `C-h' to DEL
(keyboard-translate ?\C-? ?\C-h)  ; translate DEL to `C-h'.

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

(when os-linux-p
  ;; Windows キーを単独で押して離した時の "<M-f1> is undefined" というエラーがウザいので黙らせる。
  (define-key global-map (kbd "<M-f1>") 'ignore))

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
;;(define-key global-map (kbd "C-z") ctl-z-map)
(define-key global-map (kbd "C-,") 'backward-word)
(define-key global-map (kbd "C-.") 'forward-word)

;; custom of the Esc-? key
(define-key esc-map (kbd "SPC") 'cycle-spacing)
(define-key esc-map (kbd "+") 'eval-expression)
(define-key esc-map (kbd "<") 'my-beginning-of-buffer)
(define-key esc-map (kbd ">") 'my-end-of-buffer)
(define-key esc-map (kbd "?") 'help)
(define-key esc-map (kbd "0") 'tab-close)
(define-key esc-map (kbd "2") 'tab-new)
(define-key esc-map (kbd "O") 'tab-previous)
;;(define-key esc-map (kbd "a") 'backward-sentence)
(define-key esc-map (kbd "b") 'backward-word)
(define-key esc-map (kbd "c") 'compile)
;;(define-key esc-map (kbd "d") 'kill-word)
;;(define-key esc-map (kbd "e") 'forward-sentence)
(define-key esc-map (kbd "f") 'forward-word)
;;(define-key esc-map (kbd "g") goto-map)
(define-key esc-map (kbd "h") nil)
;;(define-key esc-map (kbd "i") 'tab-to-tab-stop)
;;(define-key esc-map (kbd "j") 'indent-new-comment-line)
(define-key esc-map (kbd "k") 'kill-current-line)
(define-key esc-map (kbd "l") nil)	;; downcase-word
;;(define-key esc-map (kbd "m") 'back-to-indentation)
(define-key esc-map (kbd "n") 'forward-paragraph)
(define-key esc-map (kbd "o") 'tab-next)
(define-key esc-map (kbd "p") 'backward-paragraph)
;;(define-key esc-map (kbd "q") 'fill-paragraph)
(define-key esc-map (kbd "r") 'toggle-read-only)
;;(define-key esc-map (kbd "s") search-map)
;;(define-key esc-map (kbd "t") 'transpose-words)
;;(define-key esc-map (kbd "u") 'upcase-word)
;;(define-key esc-map (kbd "v") 'scroll-down-command)
;;(define-key esc-map (kbd "w") 'kill-ring-save)
;;(define-key esc-map (kbd "x") 'execute-extended-command)
(define-key esc-map (kbd "y") 'my-yank-pop)
;;(define-key esc-map (kbd "z") 'zap-to-char)

;; custom of the Super-? key (see term/ns-win.el)
(define-key global-map (kbd "s-[") 'tab-bar-history-back)
(define-key global-map (kbd "s-]") 'tab-bar-history-forward)
(define-key global-map (kbd "s-{") 'tab-previous)
(define-key global-map (kbd "s-}") 'tab-next)
(define-key global-map (kbd "s-0") 'delete-window)
(define-key global-map (kbd "s-1") 'delete-other-windows)
(define-key global-map (kbd "s-2") 'split-window-below)
(define-key global-map (kbd "s-3") 'split-window-right)
(define-key global-map (kbd "s-9") 'delete-other-windows-vertically)
(define-key global-map (kbd "s-a") 'helm-imenu)
(define-key global-map (kbd "s-b") 'my-helm-bookmark)
;;(define-key global-map (kbd "s-c") nil)
;;(define-key global-map (kbd "s-d") nil)
(define-key global-map (kbd "s-e") 'grep)
;;(define-key global-map (kbd "s-f") nil)
;;(define-key global-map (kbd "s-g") nil)
(define-key global-map (kbd "s-h") (lambda (arg) (interactive "p") (scroll-left arg t)))
;;(define-key global-map (kbd "s-i") nil)
(define-key global-map (kbd "s-j") 'scroll-up-one-line)
(define-key global-map (kbd "s-k") 'scroll-down-one-line)
(define-key global-map (kbd "s-l") (lambda (arg) (interactive "p") (scroll-right arg t)))
;;(define-key global-map (kbd "s-m") nil)
;;(define-key global-map (kbd "s-n") nil)
(define-key global-map (kbd "s-o") 'helm-occur)
;;(define-key global-map (kbd "s-p") nil)
;;(define-key global-map (kbd "s-q") nil)
;;(define-key global-map (kbd "s-r") nil)
(define-key global-map (kbd "s-s") 'helm-swoop)
(define-key global-map (kbd "s-t") 'tab-new)
;;(define-key global-map (kbd "s-u") nil)
(define-key global-map (kbd "s-v") 'yank) ;; for Alfred Snippets
;;(define-key global-map (kbd "s-w") nil)
;;(define-key global-map (kbd "s-x") nil)
;;(define-key global-map (kbd "s-y") nil)
;;(define-key global-map (kbd "s-z") nil)
(define-key global-map (kbd "s-J") 'scroll-up-one-line-both-window)
(define-key global-map (kbd "s-K") 'scroll-down-one-line-both-window)

;; custom of the ctl-q-map
(defvar ctl-q-map (make-keymap))
(define-key global-map (kbd "C-q") ctl-q-map)
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
(define-key ctl-q-map (kbd "C-i") 'toggle-window-division)
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
(define-key ctl-q-map (kbd "C-w") 'erase-buffer)
;;(define-key ctl-q-map (kbd "C-x") nil)
(define-key ctl-q-map (kbd "C-y") 'quote-yank)
;;(define-key ctl-q-map (kbd "C-z") nil)
(define-key ctl-q-map (kbd "DEL") 'flyspell-region)
(define-key ctl-q-map (kbd "C-SPC") 'comint-dynamic-complete-filename)

;; custom of the ctl-x-map
(define-key ctl-x-map (kbd "9") 'delete-other-windows-vertically)
(define-key ctl-x-map (kbd "M") 'compose-mail)
(define-key ctl-x-map (kbd "f") 'find-file-literally)
(define-key ctl-x-map (kbd "m") mule-keymap)
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
;;(define-key ctl-x-map (kbd "C-q") 'read-only-mode)
;;(define-key ctl-x-map (kbd "C-r") 'find-file-read-only)
;;(define-key ctl-x-map (kbd "C-s") 'save-buffer)
;;(define-key ctl-x-map (kbd "C-t") 'transpose-lines)
;;(define-key ctl-x-map (kbd "C-u") 'upcase-region)
;;(define-key ctl-x-map (kbd "C-v") 'find-alternate-file)
;;(define-key ctl-x-map (kbd "C-w") 'write-file)
;;(define-key ctl-x-map (kbd "C-x") 'exchange-point-and-mark)
;;(define-key ctl-x-map (kbd "C-y") nil)
(define-key ctl-x-map (kbd "C-z") nil)

;; custom of the ctl-z-map
(defvar ctl-z-map (make-keymap))
(define-key global-map (kbd "C-z") ctl-z-map)
(define-key ctl-z-map (kbd "0") 'my-tab-select-last)
(define-key ctl-z-map (kbd "1") 'tab-select)
(define-key ctl-z-map (kbd "2") 'tab-select)
(define-key ctl-z-map (kbd "3") 'tab-select)
(define-key ctl-z-map (kbd "4") 'tab-select)
(define-key ctl-z-map (kbd "5") 'tab-select)
(define-key ctl-z-map (kbd "6") 'tab-select)
(define-key ctl-z-map (kbd "7") 'tab-select)
(define-key ctl-z-map (kbd "8") 'tab-select)
(define-key ctl-z-map (kbd "9") 'tab-select)
(define-key ctl-z-map (kbd "k") 'tab-close)
(define-key ctl-z-map (kbd "C-b") (lambda () (interactive) (tab-move -1)))
(define-key ctl-z-map (kbd "C-c") 'tab-new)
(define-key ctl-z-map (kbd "C-f") 'tab-move)
(define-key ctl-z-map (kbd "C-j") 'my-tab-select)
(define-key ctl-z-map (kbd "C-k") 'tab-close)
(define-key ctl-z-map (kbd "C-l") 'my-tab-clone)
(define-key ctl-z-map (kbd "C-n") 'tab-next)
(define-key ctl-z-map (kbd "C-p") 'tab-previous)
(define-key ctl-z-map (kbd "C-SPC") 'tab-recent)

;;; Local Variables:
;;; tab-width: 8
;;; End:
