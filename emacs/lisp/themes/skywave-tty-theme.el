;;; skywave-tty-theme.el -- Custom Emacs Theme
;;; P.C. Shyamshankar "sykora" <sykora@lucentbeing.com>

;;; Commentary:

;;; This is a reasonable port of my vim colorscheme, which can be
;;; found in the vim section of my configuration repository. Some
;;; changes have been made, but they're fundamentally the same.

;;; This is the TTY version, where I use the 256-color support of most
;;; sane terminal emulators. For the 24-bit GUI version, see
;;; `skywave-gui-theme.el'

;;; Overrides for non-built-in faces are done on an as-needed basis.

;;; Code:

(deftheme skywave-tty "The Skywave TTY Emacs Theme")

(custom-theme-set-faces
 'skywave-tty

 ;; Built-In Faces

 '(font-lock-builtin-face ((t (:foreground "color-38"))))
 '(font-lock-comment-face ((t (:foreground "color-243"))))
 '(font-lock-constant-face ((t (:foreground "color-168"))))
 '(font-lock-doc-face ((t (:foreground "color-209"))))
 '(font-lock-function-name-face ((t (:foreground "color-229"))))
 '(font-lock-keyword-face ((t (:foreground "color-223"))))
 '(font-lock-preprocessor-face ((t (:foreground "color-197"))))
 '(font-lock-string-face ((t (:foreground "color-147")))
 '(font-lock-type-face ((t (:foreground "color-216"))))
 '(font-lock-variable-name-face ((t (:foreground "color-154"))))))

(provide-theme 'skywave-tty)
;;; skywave-tty-theme.el ends here
