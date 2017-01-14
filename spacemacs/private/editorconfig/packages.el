(defconst editorconfig-packages '(editorconfig))

(defun editorconfig/init-editorconfig ()
  (use-package runtests
    :init
    (progn
      (editorconfig-mode 't)
      (spacemacs|diminish editorconfig-mode " ⓒ" " y"))))
