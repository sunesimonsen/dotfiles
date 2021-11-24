;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; UI
(setq
 doom-font (font-spec :family "Menlo" :size 16)
 display-line-numbers-type nil
 ;; Disable help mouse-overs for mode-line segments (i.e. :help-echo text).
 ;; They're generally unhelpful and only add confusing visual clutter.
 mode-line-default-help-echo nil
 show-help-function nil
 mac-right-option-modifier 'meta
 ns-right-option-modifier  'meta)

;; Company
(add-to-list '+company-backend-alist '(js-mode company-capf company-dabbrev))

(after! company
  (map! :i "C-x C-l" 'evil-complete-previous-line)

  (setq
   ;; allow code completion inside comments and string
   company-dabbrev-code-everywhere t
   ;; Manual auto completion with C-Space
   company-idle-delay nil
   ;; allow code completion matching all buffer
   company-dabbrev-code-other-buffers 'all
   company-dabbrev-other-buffers 'all
   ))

;; Evil
(setq
 evil-escape-key-sequence "fd"
 evil-snipe-scope 'whole-visible
 evil-ex-search-persistent-highlight nil)

;; start maximized
(add-hook! window-setup #'toggle-frame-maximized)

;; æøå
(after! evil
  (map!
   :i "M-'" '(lambda () (interactive) (insert "æ"))
   :i "M-\"" '(lambda () (interactive) (insert "Æ"))
   :i "M-o" '(lambda () (interactive) (insert "ø"))
   :i "M-O" '(lambda () (interactive) (insert "Ø"))
   :i "M-a" '(lambda () (interactive) (insert "å"))
   :i "M-A" '(lambda () (interactive) (insert "Å"))
   )
  )

(after! link-hint
  (map! :leader
        :n "s c" 'link-hint-copy-link))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode ))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-jsx-mode))


(use-package! add-node-modules-path
  :hook ((js-mode js-mode js-jsx-mode typescript-mode json-mode graphql-mode) . add-node-modules-path))

(use-package! prettier-js
  :hook ((js-mode js-mode js-jsx-mode typescript-mode json-mode graphql-mode) . prettier-js-mode))

;; Projectile
(after! projectile
  (appendq! projectile-other-file-alist '(("js" "spec.js") ("spec.js" "js"))))

;; Avy
(setq avy-all-windows t)

(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

(after! evil-snipe
  (map! :nm "s" #'evil-avy-goto-word-1
        :nm "S" #'evil-avy-goto-char-timer))

;; window management
(after! winum
  (map! :leader
        :n "0"      #'winum-select-window-0-or-10
        :n "1"      #'winum-select-window-1
        :n "2"      #'winum-select-window-2
        :n "3"      #'winum-select-window-3
        :n "4"      #'winum-select-window-4
        :n "5"      #'winum-select-window-5
        :n "6"      #'winum-select-window-6
        :n "7"      #'winum-select-window-7
        :n "8"      #'winum-select-window-8
        :n "9"      #'winum-select-window-9))

;; evil-walk-on-the-edge
(after! evil
  (evil-define-motion evil-move-forward-paren (count)
    "Move forward to next (, [, {, }, ] or )"
    :jump t
    :type inclusive
    (interactive "<c>")
    (setq count (or count 1))
    (forward-char)
    (re-search-forward "\\s(\\|\\s)" nil 'end-of-buffer count)
    (while (and (in-string-p) (< (point) (point-max)))
      (re-search-forward "\\s(\\|\\s)" nil 'end-of-buffer))
    (backward-char))

  (evil-define-motion evil-move-backward-paren (count)
    "Move backward to previous (, [, {, }, ] or )"
    :jump t
    :type inclusive
    (interactive "<c>")
    (setq count (or count 1))
    (re-search-backward "\\s(\\|\\s)" nil 'beginning-of-buffer count)
    (while (and (in-string-p) (> (point) (point-min)))
      (re-search-backward "\\s(\\|\\s)")))

  (map!
   :nm "C-;" #'evil-move-backward-paren
   :nm "C-'" #'evil-move-forward-paren))

;; runtests

(use-package! runtests
  :commands runtests
  :init
  (map! :leader :nm "ct" #'runtests))


;; bufshow
(use-package! bufshow
  :commands bufshow-slideshow
  :init
  (defun bufshow-slideshow ()
    (interactive)
    (require 'bufshow)
    (setq bufshow--winconfig nil)
    (bufshow-mode t)
    (bufshow-load (buffer-file-name)))

  (map! :leader
        :prefix ("ap" . "Presentation")
        "s" #'bufshow-slideshow
        "q" #'bufshow-stop)

  (map! :map bufshow-mode-map
        "C->" #'bufshow-next
        "C-<" #'bufshow-prev))

;; EShell

;; (eval-after-load 'eshell
;;   '(progn
;;      (add-to-list 'eshell-visual-options '("jest" "--watch"))
;;      (add-to-list 'eshell-visual-subcommands '("git" "hist" "diff" "log" "show"))
;;      ))

;; Org

(after! org
  (setq
   org-capture-templates
   '(("t" "Todo" entry (file+headline "" "Tasks")
      "* TODO %?\n  %u")
     ("l" "Todo (location)" entry (file+headline "" "Tasks")
      "* TODO %?\n  %u\n  %a")
     ("u" "Todo (url)" entry (file+headline "" "Tasks")
      "* TODO %?\n  %u\n  %^C")
     ("m" "Todo (mail)" entry (file+headline "" "Tasks")
      "* TODO %?%a\n  %u"))

   org-cycle-separator-lines 0
   org-directory "~/Dropbox/org"
   org-roam-directory "~/Dropbox/org/roam"
   org-roam-index-file "~/Dropbox/org/roam/index.org"
   org-agenda-files '("~/Dropbox/org/notes.org")
   org-default-notes-file "~/Dropbox/org/notes.org"
   org-refile-targets '((org-agenda-files :level . 1))
   org-refile-use-outline-path nil
   org-agenda-custom-commands
   '(("i" "Inbox" tags-todo "+inbox")
     ("j" "Agenda and all TODOs"
      ((agenda "")
       (tags-todo "+inbox")
       (tags-todo "+zendesk")
       (tags-todo "+home")
       (tags-todo "+unexpected")
       (tags-todo "+buy")))))

  (defun org-agenda-show-agenda-and-todo (&optional arg)
    "Agenda overview"
    (interactive "P")
    (org-agenda arg "j"))

  (map! :leader
        :desc "Agenda overview"
        "oo" 'org-agenda-show-agenda-and-todo))

;; Disable smartparens
(add-hook! smartparens-enabled
  (turn-off-smartparens-mode))

;; Email
(add-to-list 'load-path "/usr/local/Cellar/mu/1.2.0_1/share/emacs/site-lisp/mu/mu4e")

(map! :leader "a m" '=mu4e)

(add-hook! evil-collection-setup
  (map! :map (mu4e-headers-mode-map mu4e-view-mode-map)
        :n "T" 'mu4e-headers-mark-thread
        :n "L" 'mu4e-jump-to-list))

(after! notmuch
  (require 'smtpmail)

  (set-popup-rule! "^\\*notmuch-hello" :ignore t)

  (setq notmuch-message-headers-visible t
        message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
        smtpmail-smtp-service 587
        smtpmail-debug-info t
        notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox not tag:deleted" :key "i")
          (:name "todo" :query "tag:inbox and tag:todo" :key "t")
          (:name "work" :query "tag:work and tag:inbox" :key "w")
          (:name "waiting" :query "tag:waiting" :key "h")
          (:name "personal" :query "tag:personal and tag:inbox" :key "p")
          (:name "support" :query "tag:support and tag:inbox" :key "u")
          (:name "jira" :query "tag:jira and tag:inbox" :key "j")
          (:name "calendar" :query "tag:calendar and tag:inbox" :key "c")
          (:name "github" :query "tag:github and tag:inbox" :key "g")
          (:name "flagged" :query "tag:flagged" :key "f")
          (:name "sent" :query "tag:sent" :key "s")
          (:name "drafts" :query "tag:draft" :key "d"))
        +notmuch-sync-backend 'offlineimap
        notmuch-fcc-dirs
        '(("ssimonsen@zendesk" . "\"Work/gmail.Sent Mail\" +sent")
          (".*" . "Personal/INBOX.Sent +sent"))
        +notmuch-home-function (lambda () (notmuch-search "tag:inbox not tag:deleted"))
        )

  (defun notmuch/send-mail-with-one ()
    (interactive)
    (setq smtpmail-smtp-user "sune@we-knowhow.dk"
          smtpmail-starttls-credentials '(("send.one.com" 587 nil nil))
          smtpmail-default-smtp-server "send.one.com"
          smtpmail-smtp-server "send.one.com"))

  (defun notmuch/send-mail-with-zendesk ()
    (interactive)
    (setq smtpmail-smtp-user "ssimonsen@zendesk.com"
          smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
          smtpmail-default-smtp-server "smtp.gmail.com"
          smtpmail-smtp-server "smtp.gmail.com"))

  (defun notmuch/message-select-mail-dest ()
    (cond ((string-match "ssimonsen@zendesk.com"
                         (message-field-value "From"))
           (notmuch/send-mail-with-zendesk))
          (t
           (notmuch/send-mail-with-one))))

  (notmuch/send-mail-with-one)

  (add-hook 'message-send-hook 'notmuch/message-select-mail-dest)

  (setq shr-color-visible-luminance-min 60)
  (setq shr-color-visible-distance-min 5)
  (setq shr-use-colors nil)
  (advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))

  (setq notmuch-multipart/alternative-discouraged '("text/plain" "text/x-amp-html"))
  )
