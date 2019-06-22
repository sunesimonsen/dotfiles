(setq hippie-exp-packages '(hippie-exp))

(defun hippie-exp/init-hippie-exp ()
  (defun smart-tab ()
    "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
    (interactive)
    (if (minibufferp)
        (unless (minibuffer-complete)
          (hippie-expand nil))
      (if mark-active
          (indent-region (region-beginning)
                         (region-end))
        (hippie-expand nil)
        )))

  (global-set-key (kbd "TAB") 'smart-tab)

  (setq hippie-expand-try-functions-list
        '(
          ;; Try to expand word "dynamically", searching the current buffer.
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching all other buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to complete text as a file name.
          try-complete-file-name
          ))

  (when (configuration-layer/package-usedp 'yasnippet)
    ;; Try to expand yasnippet snippets based on prefix
    (push 'yas-hippie-try-expand hippie-expand-try-functions-list)))
