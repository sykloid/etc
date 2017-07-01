;; An Emacs Initialization
;; P.C. Shyamshankar
;; This is rewrite #5

;; * Meta Initialization
;; ** Package System
(eval-and-compile (require 'package))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(setq package-enable-at-startup nil)
(eval-and-compile (package-initialize))

;; ** Utility Forms
;; *** Set Forms
;; The customization system is great for option discovery, but lousy for programmatic configuration.
;; Nevertheless, it has become the de-facto standard for specifying user-facing options, particuarly
;; using custom set logic. It's impossible to escape.
(defalias 'setl 'setq "Set a variable (buffer) locally.")
(defmacro setg (variable value)
  "Set a variable globally."
  `(customize-set-variable ',variable ,value))

;; *** Hook Forms
;; This macro solves multiple problems:
;;   1. It removes the need to remember `add-hook' syntax.
;;   2. It permits (but does not require) naming a hook entry, for removal afterwards. If a name is
;;      not given, one will be generated based on the hook being added to.
;;   3. It allows adding to hooks of functions which take arguments.
(defmacro add-hook+ (hook-spec arg-spec &rest body)
  "Multi-purpose hook handler."
  (declare (indent 2))
  (let* ((separator "/:")
         (components (split-string (symbol-name hook-spec) separator))
         (hook (if (= 1 (length components))
                   (car components)
                 (mapconcat #'identity (butlast components) separator)))
         (name (if (= 1 (length components))
                   (symbol-name (cl-gensym (concat hook separator)))
                 (car (last components)))))
    `(add-hook (intern ,hook) (defalias (intern ,name) (lambda ,arg-spec ,@body)) t)))

;; ** Path Initialization
(defvar user-lisp-directory (concat user-emacs-directory "lisp/"))
(setg custom-theme-directory (concat user-lisp-directory "themes/"))
(setg custom-file (concat user-emacs-directory "customizations.el"))
