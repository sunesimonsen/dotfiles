;;; packages.el --- prettier-js Layer packages File for Spacemacs
;;
;; Author: Sune Simonsen <sune@we-knowhow.dk>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq prettier-js-packages
      '((prettier-js :location local)))

(defun prettier-js/init-prettier-js ()
  (require 'prettier-js)
  ;; (add-hook 'js-mode-hook
  ;;           (lambda ()
  ;;             (add-hook 'before-save-hook 'prettier nil 'make-it-local)))
  ;; (add-hook 'js-jsx-mode-hook
  ;;           (lambda ()
  ;;             (add-hook 'before-save-hook 'prettier nil 'make-it-local)))
  )
