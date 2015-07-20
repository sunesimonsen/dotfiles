;;; packages.el --- yasnippet Layer packages File for Spacemacs
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
(setq yasnippet-packages '(yasnippet))


(defun yasnippet/init-yasnippet ()
  (use-package yasnippet
    :config
    (progn
      (let ((private-yas-dir (concat
                              configuration-layer-private-directory
                              "snippets/")))
        (setq yas-snippet-dirs (list private-yas-dir)))
      (yas-global-mode t)
      (spacemacs|diminish yas-minor-mode " ⓨ" " y"))
    )
  )
