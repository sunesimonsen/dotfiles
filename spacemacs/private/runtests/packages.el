;;; packages.el --- runtests Layer packages File for Spacemacs
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
(setq runtests-packages '(runtests))

(defun runtests/init-runtests ()
  (message "init runtests")
  (use-package runtests
    :defer t
    :commands (runtests)
    :init
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

      ;; Make ffap use line numbers
      (defvar ffap-file-at-point-line-number nil
        "Variable to hold line number from the last `ffap-file-at-point' call.")

      (defadvice ffap-file-at-point (after ffap-store-line-number activate)
        "Search `ffap-string-at-point' for a line number pattern and
save it in `ffap-file-at-point-line-number' variable."
        (let* ((string (ffap-string-at-point)) ;; string/name definition copied from `ffap-string-at-point'
               (name
                (or (condition-case nil
                        (and (not (string-match "//" string)) ; foo.com://bar
                             (substitute-in-file-name string))
                      (error nil))
                    string))
               (line-number-string
                (and (string-match ":[0-9]+" name)
                     (substring name (1+ (match-beginning 0)) (match-end 0))))
               (line-number
                (and line-number-string
                     (string-to-number line-number-string))))
          (if (and line-number (> line-number 0))
              (setq ffap-file-at-point-line-number line-number)
            (setq ffap-file-at-point-line-number nil))))

      (defadvice find-file-at-point (after ffap-goto-line-number activate)
        "If `ffap-file-at-point-line-number' is non-nil goto this line."
        (when ffap-file-at-point-line-number
          (goto-line ffap-file-at-point-line-number)
          (setq ffap-file-at-point-line-number nil)))
      (evil-leader/set-key "ot" 'runtests))
    )
  )

;; For each package, define a function runtests/init-<package-name>
;;
;; (defun runtests/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
