;;; packages.el --- terminal-here layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sune Simonsen <sunesimonsen@11371-ssimonsen.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `terminal-here-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `terminal-here/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `terminal-here/pre-init-PACKAGE' and/or
;;   `terminal-here/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst terminal-here-packages
  '(terminal-here))

(defun terminal-here/init-terminal-here ()
  (use-package terminal-here
    :defer t
    :init
    (progn
      (require 'terminal-here)

      ;; (defun open-iterm (dir)
      ;;   (cond
      ;;    ((eq system-type 'darwin)
      ;;     (list "open" "-a" "iTerm.app" dir))
      ;;    (t (terminal-here-default-terminal-command dir))))

      ;; (setq terminal-here-terminal-command 'open-iterm)

      (define-key evil-motion-state-map (kbd "SPC \"") 'terminal-here))))

;;; packages.el ends here
