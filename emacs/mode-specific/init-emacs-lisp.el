(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode t)))
(set-default 'flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(provide 'init-emacs-lisp)
