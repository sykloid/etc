;; Mode Dispatch.

(add-hook 'c-mode-common-hook (lambda () (load-library "init-c-common")))

(add-hook 'emacs-lisp-mode-hook (lambda () (load-library "init-emacs-lisp")))

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-hook 'haskell-mode-hook (lambda () (load-library "init-haskell")))

(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))
(add-hook 'html-mode-hook (lambda () (load-library "init-html")))

(add-to-list 'auto-mode-alist '("\\.ldg$" . ledger-mode))
(add-hook 'ledger-mode-hook (lambda () (load-library "init-ledger")))

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-hook 'org-mode-hook (lambda () (load-library "init-org")))

(add-hook 'python-mode-hook (lambda () (load-library "init-python")))

(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))

(provide 'init-detect)
