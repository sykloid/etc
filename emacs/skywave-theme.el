;;; skywave-theme.el -- Custom Emacs Theme
;;; P.C. Shyamshankar 'sykloid' <shyam@sykloid.org>

;;; Commentary:

;;; An evolution of my vim colorscheme, written to operate under truecolor.

(deftheme skywave "The Skywave Emacs Theme")

(custom-theme-set-faces
 'skywave
 '(font-lock-builtin-face ((t :foreground "#56AEE8")))
 '(font-lock-comment-face ((t :foreground "#666666")))
 '(font-lock-constant-face ((t :foreground "#C45271")))
 '(font-lock-doc-face ((t :foreground "#FFA8BD")))
 '(font-lock-function-name-face ((t :foreground "#F5F3B8")))
 '(font-lock-keyword-face ((t :foreground "#36B9C2")))
 '(font-lock-preprocessor-face ((t :foreground "#F0355A")))
 '(font-lock-string-face ((t :foreground "#CEA3F7")))
 '(font-lock-type-face ((t :foreground "#FAC78C")))
 '(font-lock-variable-name-face ((t :foreground "#4ED998")))

 '(outline-1 ((t (:foreground "#FAC78C"))))
 '(outline-2 ((t (:foreground "#6EB8F5"))))
 '(outline-3 ((t (:foreground "#C45271"))))
 '(outline-4 ((t (:foreground "#4ED998"))))

 '(region ((t (:background "#444444"))))

 '(vertico-current ((t (:background "#333333"))))
 '(vertico-group-title ((t (:background "#222222"))))
 '(vertico-group-separator ((t (:background "#222222"))))

 '(highlight ((t (:background "#333333"))))
 '(isearch ((t (:foreground "#FE0012" :background "#DFDFDF"))))
 '(lazy-highlight ((t (:background "#777777")))))

(provide-theme 'skywave)
