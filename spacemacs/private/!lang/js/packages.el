;;; packages.el --- js Layer packages File for Spacemacs
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
(setq js-packages '(js compile projectile flycheck))

(defun js/post-init-flycheck ()
  (add-hook 'js-mode-hook 'flycheck-mode))

(defun js/init-js ()
  (use-package js
    :defer t
    :init
    (progn
      (defun my-js-imenu-make-index ()
        (save-excursion
          (imenu--generic-function '((nil "function\\s-+\\([^ ]+\\)(" 1)
                                     (nil "\\.\\([^\\. ]+\\)\\s-*=\\s-*function\\s-*(" 1)))))

      (add-hook 'js-mode-hook
                (lambda ()
                  (setq imenu-create-index-function 'my-js-imenu-make-index)))

      (define-derived-mode jsx-mode web-mode "jsx")
      (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
      (add-hook 'jsx-mode-hook (lambda ()
                                 (emmet-mode 0)))
      )
    )
  )

(defun js/init-projectile ()
  (use-package projectile
    :defer t
    :config
    (progn
      (add-to-list 'projectile-other-file-alist '("js" "spec.js"))
      ;; not supported yet but there is a issue for it
      ;; https://github.com/bbatsov/projectile/issues/454
      (add-to-list 'projectile-other-file-alist '("spec.js" "js"))
      (evil-leader/set-key-for-mode 'js-mode "mga" 'projectile-find-other-file)
      )
    )
  )

(defun js/init-compile ()
  (use-package compile
    :defer t
    :config
    (progn
      ;; Compilation mode file links for JavaScript stack traces
      (add-to-list 'compilation-error-regexp-alist 'phantomjs-stack-trace)
      (add-to-list 'compilation-error-regexp-alist-alist
                   '(phantomjs-stack-trace
                     "at \\(?:.+ \\)?(?\\(.+\\):\\([[:digit:]]+\\))?$"
                     1 2))

      (add-to-list 'compilation-error-regexp-alist 'javascript-stack-trace)
      (add-to-list 'compilation-error-regexp-alist-alist
                   '(javascript-stack-trace
                     "at \\(?:.+ \\)?(?\\(.+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\))?$"
                     1 2 3))
      )
    )
  )
