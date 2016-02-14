;;; packages.el --- css-comb Layer packages File for Spacemacs
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

(setq css-comb-packages '(css-comb))

(defun css-comb/init-css-comb ()
  (use-package css-comb
    :defer t
    :init
    (progn
      (evil-leader/set-key-for-mode 'css-mode "m=" 'css-comb)
      )
    )
  )
