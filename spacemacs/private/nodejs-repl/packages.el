;;; packages.el --- nodejs-repl Layer packages File for Spacemacs
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
(setq nodejs-repl-packages '(nodejs-repl))

(defun nodejs-repl/init-nodejs-repl ()
  (use-package nodejs-repl
    :defer t
    :init
    (progn
      (defun nodejs-repl-send-region ()
        (interactive)
        (when (use-region-p)
          (let ((selection (buffer-substring-no-properties  (region-beginning) (region-end))))
            (with-current-buffer (get-buffer "*nodejs*")
              (end-of-buffer)
              (insert selection)
              (comint-send-input)
              (end-of-buffer)))))

      (evil-leader/set-key-for-mode 'js-mode "msr" 'nodejs-repl-send-region)
      (evil-leader/set-key-for-mode 'js2-mode "msr" 'nodejs-repl-send-region)
      (evil-leader/set-key-for-mode 'js-jsx-mode "msr" 'nodejs-repl-send-region)
      )
    )
  )
