(deftheme skywave "The Skywave Emacs Theme.")

(custom-theme-set-faces
 'skywave

 ;; Font-Lock
 '(font-lock-builtin-face ((t (:foreground "color-38"))))
 '(font-lock-comment-face ((t (:foreground "color-243"))))
 '(font-lock-constant-face ((t (:foreground "color-168"))))
 '(font-lock-doc-face ((t (:foreground "color-209"))))
 '(font-lock-function-name-face ((t (:foreground "color-229"))))
 '(font-lock-preprocessor-face ((t (:foreground "color-197"))))
 '(font-lock-string-face ((t (:foreground "color-147"))))
 '(font-lock-type-face ((t (:foreground "color-216"))))
 '(font-lock-variable-name-face ((t (:foreground "color-154"))))

 ;; Interface
 '(lazy-highlight ((t (:foreground "color-233" :background "color-104"))))
 '(region ((t (:background "color-204" :foreground "color-232"))))

 ;; Mode-Line
 '(mode-line ((t (:foreground "color-249" :background "color-234"))))
 '(which-func ((t (:foreground "color-39"))))

 ;; Org-Mode
 '(org-level-1 ((t (:foreground "color-168"))))
 '(org-level-2 ((t (:foreground "color-111"))))
 '(org-level-3 ((t (:foreground "color-216"))))
 '(org-level-4 ((t (:foreground "color-79"))))
 '(org-special-keyword ((t (:foreground "color-119"))))
 )

(provide-theme 'skywave)
