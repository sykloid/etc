(turn-on-haskell-decl-scan)
(turn-on-haskell-doc-mode)
(turn-on-haskell-indentation)

(set-default 'haskell-indentation-left-offset 4)
(set-default 'haskell-indentation-where-pre-offset 2)
(set-default 'haskell-indentation-where-post-offset 2)

;; Almost every comment in haskell code is haddock'd anyway, so we don't need a doc-face.
(face-remap-add-relative 'font-lock-doc-face 'font-lock-comment-face)

(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)

(provide 'init-haskell)
