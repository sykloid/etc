;;; skywave-theme.el -- Custom Emacs Theme
;;; P.C. Shyamshankar "sykora"  <sykora@lucentbeing.com>

;;; This is an evolution of my vim colorscheme, which is available in the vim section of my
;;; configuration repository. I used to have separate `-tty' and `-gui' versions, but then I
;;; discovered more arguments to`defface'.

;;; Overrides for non-builtin faces are provided as needed.

(deftheme skywave "The Skywave Emacs Theme")

;; `min-colors' compares as less-than, options MUST appear in descending order to dispatch
;; correctly.
(defconst skywave-gui '((min-colors 16777216)))
(defconst skywave-tty '((min-colors 256)))

(custom-theme-set-faces
 'skywave

 `(font-lock-builtin-face
   ((,skywave-gui . (:foreground "#56AEE8"))
    (,skywave-tty . (:foreground "color-38"))))
 `(font-lock-comment-face
   ((,skywave-gui . (:foreground "#666666"))
    (,skywave-tty . (:foreground "color-243"))))
 `(font-lock-constant-face
   ((,skywave-gui . (:foreground "#C45271"))
    (,skywave-tty . (:foreground "color-168"))))
 `(font-lock-doc-face
   ((,skywave-gui . (:foreground "#FFA8BD"))
    (,skywave-tty . (:foreground "color-180"))))
 `(font-lock-function-name-face
   ((,skywave-gui . (:foreground "#F5F3B8"))
    (,skywave-tty . (:foreground "color-229"))))
 `(font-lock-keyword-face
   ((,skywave-gui . (:foreground "#36B9C2"))
    (,skywave-tty . (:foreground "color-80"))))
 `(font-lock-keyword-face
   ((,skywave-gui . (:foreground "#36B9C2"))
    (,skywave-tty . (:foreground "color-223"))))
 `(font-lock-preprocessor-face
   ((,skywave-gui . (:foreground "#F0355A"))
    (,skywave-tty . (:foreground "color-197"))))
 `(font-lock-string-face
   ((,skywave-gui . (:foreground "#CEA3F7"))
    (,skywave-tty . (:foreground "color-147"))))
 `(font-lock-type-face
   ((,skywave-gui . (:foreground "#FAC78C"))
    (,skywave-tty . (:foreground "color-216"))))
 `(font-lock-variable-name-face
   ((,skywave-gui . (:foreground "#4ED998"))
    (,skywave-tty . (:foreground "color-121"))))

 `(highlight
   ((,skywave-gui . (:background "#444444")))
   ((,skywave-tty . (:background "color-233"))))

 `(region
   ((,skywave-gui . (:background "#444444")))
   ((,skywave-tty . (:background "color-233"))))

 `(error
   ((,skywave-gui . (:foreground "#F4429E"))
    (,skywave-tty . (:foreground "color-162"))))

 `(warning
   ((,skywave-gui . (:foreground "#F2AA63"))
    (,skywave-tty . (:foreground "color-214"))))

 ;; Compilation
 `(compilation-info
   ((,skywave-gui . (:foreground "#63D5F2"))))

 `(compilation-line-number
   ((,skywave-gui . (:foreground "#1ACCAB"))))

 `(compilation-column-number
   ((,skywave-gui . (:foreground "#1ACCAB"))))

 ;; Diff
 `(diff-added
   ((,skywave-gui . (:foreground "Aquamarine2"))
    (,skywave-tty . (:foreground "color-43"))))
 `(diff-removed
   ((,skywave-gui . (:foreground "PaleVioletRed2"))
    (,skywave-tty . (:foreground "color-204"))))

 ;; Magit
 `(magit-diff-added
   ((t . (:inherit diff-added))))
 `(magit-diff-removed
   ((t . (:inherit diff-removed))))

 `(magit-diff-added-highlight
   ((default . (:inherit magit-diff-added))
    (,skywave-gui . (:background "#333333"))
    (,skywave-tty . (:background "color-235"))))

 `(magit-diff-removed-highlight
   ((default . (:inherit magit-diff-removed))
    (,skywave-gui . (:background "#333333"))
    (,skywave-tty . (:background "color-235"))))

 `(magit-diff-context-highlight
   ((,skywave-gui . (:background "#333333"))
    (,skywave-tty . (:background "color-235"))))

 ;; OCaml
 `(merlin-type-face
   ((,skywave-gui . (:foreground "#C3FF87" :background "#A3A3A3"))))

 ;; Org
 '(org-bold ((t (:foreground "#EA7EAF" :weight ultra-bold))))
 '(org-italic ((t (:foreground "#7EEABF" :slant italic))))
 '(org-underline ((t (:underline t))))
 '(org-verbatim ((t (:foreground "#F0E9A1"))))
 '(org-code ((t (:foreground "#69ACC8"))))
 '(org-strike-through ((t (:strike-through t))))

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

(provide-theme 'skywave)
