;;; packages.el --- prettier-js Layer packages File for Spacemacs
;;
;; Author: Sune Simonsen <sune@we-knowhow.dk>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq prettier-js-packages
      '(prettier-js))

(defun prettier-js/setup-local-prettier ()
  "If prettier found in node_modules directory - use that.
Intended for use in PROJECTILE-AFTER-SWITCH-PROJECT-HOOK."
  (interactive)

  (let* ((prettier-bin "node_modules/.bin/prettier")
         (project-dir
          (locate-dominating-file
           buffer-file-name
           (lambda (parent) (file-exists-p (expand-file-name (concat parent prettier-bin))))))
         (local-prettier
          (and project-dir
               (expand-file-name (concat project-dir prettier-bin)))))

    (when local-prettier
      (make-local-variable 'prettier-command)

      (setq prettier-js-command local-prettier)
      (spacemacs|diminish prettier-js-mode " Ⓟ" " P")

      (prettier-js-mode))))

(defun prettier-js/init-prettier-js ()
  (require 'prettier-js)

  (mapc (lambda (hook)
          (add-hook
           hook
           (lambda ()
             (prettier-js/setup-local-prettier))))
        '(js-jsx-mode-hook js-mode-hook js2-mode-hook)))
