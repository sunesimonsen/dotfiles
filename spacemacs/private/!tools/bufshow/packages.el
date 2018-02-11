;;; packages.el --- bufshow layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sune Simonsen <sunesimonsen@11371-ssimonsen.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst bufshow-packages
  '(bufshow))

(defun bufshow/init-bufshow ()
  (use-package bufshow
    :commands (bufshow-slideshow)
    :defer t
    :init
    (progn

      (defun bufshow-slideshow ()
        (interactive)
        (require 'bufshow)
        (bufshow-mode t)
        (bufshow-load (buffer-file-name)))

      (spacemacs/declare-prefix "aa" "applications")
      (define-key evil-motion-state-map (kbd "SPC a a s") 'bufshow-slideshow)

      (spacemacs/set-leader-keys-for-minor-mode 'bufshow-mode
        "p" 'bufshow-prev
        "n" 'bufshow-next
        "q" 'bufshow-stop)
    )))
