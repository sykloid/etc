;;; narrow-or-widen-dwim.el - Fairly self-explanatory.
;;; Taken from `http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html'.

(autoload 'eshell-show-output "eshell")

(autoload 'org-at-block-p "org")
(autoload 'org-edit-src-code "org")
(autoload 'org-narrow-to-block "org")
(autoload 'org-narrow-to-subtree "org")

(autoload 'LaTeX-narrow-to-environment "latex")

(defun narrow-or-widen-dwim (p)
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((org-at-block-p)
                (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        ((derived-mode-p 'eshell-mode)
         (let ((current-prefix-arg '(4)))
           (call-interactively 'eshell-show-output)))
        (t (narrow-to-defun))))

(provide 'narrow-or-widen-dwim)
