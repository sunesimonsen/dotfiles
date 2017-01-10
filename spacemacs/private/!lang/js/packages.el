;;; packages.el --- js Layer packages File for Spacemacs
;;
;; Author: Sune Simonsen <sune@we-knowhow.dk>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq js-packages '(js projectile compile flycheck))

(defun js/post-init-flycheck ()
  (add-hook 'js-mode-hook 'flycheck-mode)
  (require 'flycheck)
  (flycheck-add-mode 'javascript-standard 'jsx-mode))

(defun js/setup-local-standard ()
  "If standard found in node_modules directory - use that for flycheck.
Intended for use in PROJECTILE-AFTER-SWITCH-PROJECT-HOOK."
  (interactive)
  (let ((local-standard (expand-file-name (concat (projectile-project-root) "node_modules/.bin/standard"))))
    (make-local-variable 'flycheck-javascript-standard-executable)
    (setq flycheck-javascript-standard-executable
          (and (file-exists-p local-standard) local-standard))))

(defun js/init-js ()
  (use-package js
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . js-jsx-mode))

      (defun my-js-imenu-make-index ()
        (save-excursion
          (imenu--generic-function '((nil "function\\s-+\\([^ ]+\\)(" 1)
                                     (nil "\\.\\([^\\. ]+\\)\\s-*=\\s-*function\\s-*(" 1)))))

      (add-hook
       'js-mode-hook
       (lambda ()
         (setq imenu-create-index-function 'my-js-imenu-make-index)
         (setq electric-indent-inhibit t)
         (js/setup-local-standard)))
      )
    )
  )

(defun js/post-init-projectile ()
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-other-file-alist '("js" "spec.js"))
    (add-to-list 'projectile-other-file-alist '("spec.js" "js")))

  (evil-leader/set-key-for-mode 'js-mode "mga" 'projectile-find-other-file))

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
