(setq runtests-packages '(runtests))

(defun runtests/init-runtests ()
  (use-package runtests
    :defer t
    :commands (runtests)
    :init
    (progn
      (evil-leader/set-key "ot" 'runtests))
    )
  )
