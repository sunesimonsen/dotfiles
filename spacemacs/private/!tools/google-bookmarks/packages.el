;;; packages.el --- google-bookmarks Layer packages File for Spacemacs
;;
;; Author: Sune Simonsen <sune@we-knowhow.dk>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq google-bookmarks-packages '(helm-chrome))

(defun google-bookmarks/init-helm-chrome ()
  (use-package helm-google
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "ag" "google")
      (define-key evil-motion-state-map (kbd "SPC a g b") 'helm-chrome-bookmarks)
      (define-key evil-motion-state-map (kbd "SPC a g r") 'helm-chrome-reload-bookmarks)
      )
    )
  )
