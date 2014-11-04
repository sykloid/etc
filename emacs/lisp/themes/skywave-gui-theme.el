;;; skywave-gui-theme.el -- Custom Emacs Theme
;;; P.C. Shyamshankar "sykora" <sykora@lucentbeing.com>

;;; Commentary:

;;; This is a reasonable port of my vim colorscheme, which can be
;;; found in the vim section of my configuration repository. Some
;;; changes have been made, but they're fundamentally the same.

;;; This is the GUI version, where I make use of the windowing
;;; system's support for 24-bit colors. For the 256-color TTY version,
;;; see `skywave-tty-theme.el'.

;;; Overrides for non-built-in faces are done on an as-needed basis.

;;; Code:

(deftheme skywave-gui "The Skywave GUI Emacs Theme")

(custom-theme-set-faces
 'skywave-gui

 ;; Built-In Faces
 '(font-lock-builtin-face ((t (:foreground "#56AEE8"))))
 '(font-lock-comment-face ((t (:foreground "#666666"))))
 '(font-lock-constant-face ((t (:foreground "#C45271"))))
 '(font-lock-doc-face ((t (:foreground "#E0B88D"))))
 '(font-lock-function-name-face ((t (:foreground "#F5F3B8"))))
 '(font-lock-keyword-face ((t (:foreground "#36B9C2"))))
 '(font-lock-preprocessor-face ((t (:foreground "#F0355A"))))
 '(font-lock-string-face ((t (:foreground "#CEA3F7"))))
 '(font-lock-type-face ((t (:foreground "#FAC78C"))))
 '(font-lock-variable-name-face ((t (:foreground "#4ED998"))))

 '(region ((t (:foreground nil :background "#333333"))))

 ;; Org-Mode/Outline-Mode
 '(org-code ((t (:foreground "#69ACC8"))))
 '(org-verbatim ((t (:foreground "#F0E9A1"))))

 '(outline-1 ((t (:foreground "#FAC78C"))))
 '(outline-2 ((t (:foreground "#6EB8F5"))))
 '(outline-3 ((t (:foreground "#C45271"))))
 '(outline-4 ((t (:foreground "#4ED998"))))

 '(org-date ((t (:foreground "#22D9E3" :underline nil))))
 '(org-link ((t (:foreground "#22D9E3" :underline t))))
 '(org-todo ((t (:foreground "#FFFFFF" :underline t :bold t))))
 '(org-done ((t (:foreground "#777777"))))

 '(org-upcoming-deadline ((t (:foreground "#F5F3B8"))))
 '(org-warning ((t (:foreground "#E5C5F0"))))

 '(org-agenda-structure ((t (:foreground "#8EE5EE")))))

(provide-theme 'skywave-gui)
;;; skywave-gui-theme.el ends here
