;; init-overrides -- monkey patches for the best of us.

;; For whatever reason, evil-add-hjkl-bindings has the h, j, k and l keys hard-coded into the
;; substitution table. This makes it especially difficult to rebind those particular keys, since
;; they pervade into other modes before they can be intercepted. Fortunately, we can intercept their
;; interjections.
(eval-after-load 'evil-core
  '(defmacro evil-add-hjkl-bindings (keymap &optional state &rest bindings)
    "This macro has been modified to compensate for stupidly hardcoded values."
    (declare (indent defun))
    `(evil-define-key ,state ,keymap
       "n" (lookup-key evil-motion-state-map "n")
       "e" (lookup-key evil-motion-state-map "e")
       "i" (lookup-key evil-motion-state-map "i")
       "o" (lookup-key evil-motion-state-map "o")
       ":" (lookup-key evil-motion-state-map ":")
       ,@bindings)))

;; A custom ellipsis character needs to be overridden as soon as org is loaded, due to syntax-table
;; intricacies.
(eval-after-load 'org '(set 'org-ellipsis " ↩"))

(provide 'init-overrides)
