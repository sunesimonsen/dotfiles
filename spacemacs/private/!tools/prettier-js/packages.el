;;; packages.el --- prettier-js Layer packages File for Spacemacs
;;
;; Author: Sune Simonsen <sune@we-knowhow.dk>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq prettier-js-packages
      '((prettier-js :location local)))

(defun prettier-js/setup-local-prettier ()
  "If prettier found in node_modules directory - use that.
Intended for use in PROJECTILE-AFTER-SWITCH-PROJECT-HOOK."
  (interactive)

  (let* ((project-dir
          (locate-dominating-file
           buffer-file-name
           (lambda (parent) (file-exists-p (expand-file-name (concat parent "node_modules/.bin/prettier"))))))
         (local-prettier
          (and project-dir
               (expand-file-name (concat project-dir "node_modules/.bin/prettier")))))

    (when local-prettier
      (make-local-variable 'prettier-command)

      (setq prettier-command local-prettier))))

(defun prettier-js/init-prettier-js ()
  (require 'prettier-js)

  (add-hook
   'js-jsx-mode-hook
   (lambda ()
     (prettier-js/setup-local-prettier)))
  )
