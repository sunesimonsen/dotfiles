;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; UI
(setq
 doom-font (font-spec :family "DejaVu Sans Mono" :size 16)
 display-line-numbers-type nil
 ;; Manual auto completion with C-Space
 company-idle-delay nil
 ;; Disable help mouse-overs for mode-line segments (i.e. :help-echo text).
 ;; They're generally unhelpful and only add confusing visual clutter.
 mode-line-default-help-echo nil
 show-help-function nil)

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

;; JavaScript

(use-package! js
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-jsx-mode))

  (defun my-js-imenu-make-index ()
    (save-excursion
      (imenu--generic-function '(("Function" "const\\s-+\\([^ ]+\\)\\s-*=.+=>" 0)
                                 ("Function" "function\\s-+\\([^ ]+\\)(" 0)
                                 ("Function" "\\.\\([^\\. ]+\\)\\s-*=\\s-*function\\s-*(" 0)))))
  (add-hook
   'js-jsx-mode-hook
   (lambda ()
     (setq imenu-create-index-function 'my-js-imenu-make-index))))

(use-package! add-node-modules-path
  :hook ((js-mode js-mode js-jsx-mode typescript-mode json-mode graphql-mode) . add-node-modules-path))

(use-package! prettier-js
  :hook ((js-mode js-mode js-jsx-mode typescript-mode json-mode graphql-mode) . prettier-js-mode))

;; Projectile
(after! projectile
  (appendq! projectile-other-file-alist '(("js" "spec.js") ("spec.js" "js"))))

;; Avy
(setq avy-all-windows t)

(after! evil-snipe
  (map! :map evil-snipe-local-mode-map
        :nm "s" #'evil-avy-goto-word-1
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
      "* TODO %?%:subject\n  %u\n  %a"))

   org-directory "~/Dropbox/org"
   org-agenda-files '("~/Dropbox/org/notes.org")
   org-default-notes-file "~/Dropbox/org/notes.org"))

;; Email
(add-to-list 'load-path "/usr/local/Cellar/mu/1.2.0_1/share/emacs/site-lisp/mu/mu4e")

(map! :leader "a m" '=mu4e)

(add-hook! evil-collection-setup
  (map! :map (mu4e-headers-mode-map mu4e-view-mode-map)
        :n "T" 'mu4e-headers-mark-thread))

(after! mu4e

  (setq
   +mu4e-backend 'offlineimap
   mu4e-maildir "~/Maildir"
   mu4e-user-agent-string
   mu4e-update-interval 300
   mu4e-html2text-command "w3m -dump -s -T text/html -o display_link_number=true | sed 's#^\\(\\[[[:digit:]]\\+\\]\\) -$#\\1 https://no.op#'"

   message-send-mail-function 'smtpmail-send-it
   (format "mu4e %s; Doom Emacs %s" mu4e-mu-version doom-version)
   starttls-use-gnutls t
   smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
   smtpmail-smtp-service 587
   smtpmail-debug-info t)

  (set-email-account!
   "Personal"
   '((user-mail-address      . "sune@we-knowhow.dk")
     (mu4e-sent-folder       . "/Personal/INBOX.Sent")
     (mu4e-drafts-folder     . "/Personal/INBOX.Drafts")
     (mu4e-trash-folder      . "/Personal/INBOX.Trash")
     (mu4e-refile-folder     . "/Personal/INBOX.Archive")

     (mu4e-bookmarks . (("maildir:/Personal/INBOX" "Inbox" ?i)
                        ("maildir:/Personal/INBOX.Todo" "Todo" ?t)
                        ("maildir:/Personal/INBOX.Archive" "Archive" ?a)
                        ("flag:unread AND NOT flag:trashed AND (maildir:/Personal/INBOX OR maildir:/Work/INBOX)"
                         "Unread messages" ?u)
                        ("flag:unread AND flag:list AND NOT flag:trashed AND (maildir:/Personal/INBOX OR maildir:/Work/INBOX)"
                         "Unread list" ?l)
                        ("date:today..now" "Today's messages" ?d)
                        ("date:7d..now" "Last 7 days" ?w)
                        ("mime:image/*" "Messages with images" ?p)))

     (smtpmail-smtp-user     . "sune@we-knowhow.dk")
     (smtpmail-starttls-credentials . (("send.one.com" 587 nil nil)))
     (smtpmail-default-smtp-server . "send.one.com")
     (smtpmail-smtp-server . "send.one.com"))
   t)

  (set-email-account!
   "Work"
   '((user-mail-address . "ssimonsen@zendesk.com")
     (mu4e-trash-folder . "/Work/[Gmail].Trash")
     (mu4e-refile-folder . "/Work/[Gmail].All Mail")
     (mu4e-sent-folder . "/Work/[Gmail].Sent Mail")
     (mu4e-drafts-folder . "/Work/[Gmail].Drafts")

     (mu4e-bookmarks . (("maildir:/Work/INBOX" "Inbox" ?i)
                        ("maildir:\"/Work/[Gmail].All Mail\"" "All Mail" ?a)
                        ("flag:unread AND NOT flag:trashed AND (maildir:/Personal/INBOX OR maildir:/Work/INBOX)"
                         "Unread messages" ?u)
                        ("flag:unread AND flag:list AND NOT flag:trashed AND (maildir:/Personal/INBOX OR maildir:/Work/INBOX)"
                         "Unread list" ?l)
                        ("date:today..now" "Today's messages" ?d)
                        ("date:7d..now" "Last 7 days" ?w)
                        ("mime:image/*" "Messages with images" ?p)))

     (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
     (smtpmail-default-smtp-server . "smtp.gmail.com")
     (smtpmail-smtp-server . "smtp.gmail.com"))
   t)
  )
