;;; packages.el --- evil-walk-on-the-edge Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq evil-walk-on-the-edge-packages '(evil))

(defun evil-walk-on-the-edge/init-evil ()
  (use-package evil
    :defer t
    :config
    (progn
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

      (define-key evil-motion-state-map "å" 'evil-move-backward-paren)
      (define-key evil-motion-state-map "ø" 'evil-move-forward-paren)
      )
    )
  )
