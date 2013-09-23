(require 'thingatpt)

;;; Customize
(setq custom-file "~/bin/dotfiles/evil.d/custom.el")
(load custom-file)

(setq el-get-user-package-directory "~/.emacs.d/el-get-init-files/")

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(setq
 el-get-sources
 '((:name el-get)
   (:name evil-surround)
   (:name evil-numbers)
   (:name flymake-cursor)
   (:name find-file-in-git-repo)
   (:name markdown-mode)
   (:name ace-jump-mode)
;   (:name less-css-mode)

   (:name color-theme
	  :after
	  (progn
	    (color-theme-initialize)
	    (defun light-theme ()
	      (interactive)
	      (color-theme-standard))

	    (defun dark-theme ()
	      (interactive)
	      (color-theme-billw))

	    (dark-theme)))


   (:name evil
	  :after
	  (progn
	    (evil-mode 't)

	    (evil-define-motion evil-move-forward-paren (count)
	      "Move forward to next (, [, {, }, ] or )"
	      :jump t
	      :type inclusive
	      (interactive "<c>")
	      (setq count (or count 1))
	      (forward-char)
	      (re-search-forward "\\s(\\|\\s)" nil 'end-of-buffer count)
	      (while (in-string-p)
		(re-search-forward "\\s(\\|\\s)" nil 'end-of-buffer))
	      (backward-char))

	    (evil-define-motion evil-move-backward-paren (count)
	      "Move backward to previous (, [, {, }, ] or )"
	      :jump t
	      :type inclusive
	      (interactive "<c>")
	      (setq count (or count 1))
	      (re-search-backward "\\s(\\|\\s)" nil 'beginning-of-buffer count)
	      (while (in-string-p)
		(re-search-backward "\\s(\\|\\s)")))

	    (define-key evil-motion-state-map "æ" 'evil-ex)
	    (define-key evil-normal-state-map "Æ" 'evil-execute-macro)

	    (define-key evil-motion-state-map "å" 'evil-move-backward-paren)
	    (define-key evil-motion-state-map "ø" 'evil-move-forward-paren)

	    (define-key evil-motion-state-map "Å" 'evil-backward-paragraph)
	    (define-key evil-motion-state-map "Ø" 'evil-forward-paragraph)

	    (define-key evil-motion-state-map (kbd "SPC") 'evil-search-forward)
	    (define-key evil-motion-state-map (kbd "C-SPC") 'evil-search-backward)

	    (define-key evil-visual-state-map (kbd "C-æ") 'evil-exit-visual-state)
	    (define-key evil-insert-state-map (kbd "C-æ") 'evil-normal-state)
	    (define-key evil-replace-state-map (kbd "C-æ") 'evil-normal-state)
	    (define-key evil-normal-state-map (kbd "C-æ") 'evil-force-normal-state)

	    (define-key evil-outer-text-objects-map "å" 'evil-a-bracket)
	    (define-key evil-outer-text-objects-map "ø" 'evil-a-bracket)
	    (define-key evil-outer-text-objects-map "Å" 'evil-a-curly)
	    (define-key evil-outer-text-objects-map "Ø" 'evil-a-curly)

	    (define-key evil-motion-state-map (kbd "M-h") 'evil-window-left)
	    (define-key evil-motion-state-map (kbd "M-j") 'evil-window-down)
	    (define-key evil-motion-state-map (kbd "M-k") 'evil-window-up)
	    (define-key evil-motion-state-map (kbd "M-l") 'evil-window-right)
	    (define-key evil-motion-state-map (kbd "M-n") 'evil-window-new)
	    (define-key evil-motion-state-map (kbd "M-o") 'delete-other-windows)
	    (define-key evil-motion-state-map (kbd "M-p") 'evil-window-mru)
	    (define-key evil-motion-state-map (kbd "M-c") 'delete-window)
	    (define-key evil-motion-state-map (kbd "M-v") 'split-window-horizontally)
	    (define-key evil-motion-state-map (kbd "M-s") 'split-window-vertically)
	    (define-key evil-motion-state-map (kbd "M--") 'evil-window-decrease-height)
	    (define-key evil-motion-state-map (kbd "M-+") 'evil-window-increase-height)

	    (define-key evil-inner-text-objects-map "å" 'evil-inner-bracket)
	    (define-key evil-inner-text-objects-map "ø" 'evil-inner-bracket)
	    (define-key evil-inner-text-objects-map "Å" 'evil-inner-curly)
	    (define-key evil-inner-text-objects-map "Ø" 'evil-inner-curly)

	    (define-key evil-motion-state-map "gø" 'evil-jump-to-tag)
	    (define-key evil-motion-state-map "gk" 'evil-ace-jump-word-mode)
	    (define-key evil-motion-state-map "gn" 'flymake-goto-next-error)
	    (define-key evil-motion-state-map "gp" 'flymake-goto-prev-error)

	    (define-key evil-motion-state-map "\C-ø" 'evil-jump-to-tag)

	    (define-key evil-insert-state-map (kbd "<tab>") 'yas/expand)
	    (define-key evil-visual-state-map (kbd "<tab>") ">gv")
	    (define-key evil-visual-state-map (kbd "<backtab>") "<gv")

	    (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
	    (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

	    (define-prefix-command 'evil-leader-map)
	    (define-key evil-leader-map "," 'evil-repeat-find-char)
	    (define-key evil-leader-map "p" 'find-file-in-git-repo)
	    (define-key evil-leader-map "e" 'ido-find-file)
	    (define-key evil-leader-map "b" 'ido-switch-buffer)
	    (define-key evil-leader-map "w" 'evil-write)
	    (define-key evil-motion-state-map "," 'evil-leader-map)

	    (define-key evil-motion-state-map ";" 'evil-repeat-find-char-reverse)

	    (define-key evil-normal-state-map "g," 'goto-last-change)
	    (define-key evil-normal-state-map "g;" 'goto-last-change-reverse)

	    (defun query-replace-symbol-at-point (replacement)
	      "Query replace the symbol at point with the given replacement"
	      (interactive "swith: ")
	      (beginning-of-thing 'symbol)
	      (let* ((symbol-at-point (thing-at-point 'symbol))
                 (pattern (format "\\_<%s\\_>" (regexp-quote symbol-at-point))))
            (query-replace-regexp pattern replacement)))

	    (define-key evil-normal-state-map "gs" 'query-replace-symbol-at-point)

	    (define-key minibuffer-local-map [escape] 'keyboard-escape-quit)
	    (define-key minibuffer-local-ns-map [escape] 'keyboard-escape-quit)
	    (define-key minibuffer-local-completion-map [escape]
	      'keyboard-escape-quit)
	    (define-key minibuffer-local-must-match-map [escape]
	      'keyboard-escape-quit)
	    (define-key minibuffer-local-isearch-map [escape]
	      'keyboard-escape-quit)

	    (evil-ex-define-cmd "bo[okmarks]" 'list-bookmarks)
	    (evil-ex-define-cmd "p[roject]" 'ido-project-root-find-file)
	    (evil-ex-define-cmd "b[uffer]" 'ido-switch-buffer)
	    (evil-ex-define-cmd "e[dit]" 'ido-find-file)))

   (:name smex
	  :after
	  (progn
	    ;; SMEX
	    (setq smex-save-file "~/.emacs.d/.smex-items")

	    (defun lazy-load-smex ()
	      (interactive)
	      (or (boundp 'smex-cache)
		  (smex-initialize))
	      (global-set-key [(meta x)] 'smex)
	      (global-set-key [menu] 'smex)
	      (smex))

        (global-set-key [menu] 'lazy-load-smex)
        (global-set-key [(meta x)] 'lazy-load-smex)))

   (:name yasnippet
      :after
      (progn
        (setq yas/snippet-dirs
              '("~/bin/dotfiles/evil.d/snippets"))
        (yas/global-mode 't)))

   (:name flymake-node-jshint
          :after
          (progn
            (add-hook 'js-mode-hook (lambda () (flymake-mode 1)))))
 ))

(setq my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src)))

(add-to-list 'el-get-recipe-path "~/bin/dotfiles/evil.d/recipes")
(el-get 'sync my:el-get-packages)

;;; IDo mode
(ido-mode 't)
(setq ido-enable-flex-matching 't)
(define-key ido-file-dir-completion-map (kbd "C-c C-s")
      (lambda()
        (interactive)
        (ido-initiate-auto-merge (current-buffer))))

(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

; Font
(set-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")

; stop forcing me to spell out "yes"
(fset 'yes-or-no-p 'y-or-n-p)

(define-key key-translation-map (kbd "½") (kbd "$"))

(require 'uniquify)
(set-variable 'uniquify-buffer-name-style 'forward)

;;; auto-mode-alist
(add-to-list 'auto-mode-alist '("\\.ko$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
