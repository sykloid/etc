;; -*- eval: (outline-minor-mode) -*-
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

;; ** Use-Package Bootstrap
;; `use-package' itself is only needed during byte-compilation.
(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))

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

;; ** Custom Initialization
(setg custom-file (concat user-emacs-directory "customizations.el"))

(defvar custom-safe-variables
  '(custom-safe-themes))

(defun custom-set-only-safe (args)
  (cl-remove-if-not (lambda (elt) (member (car elt) custom-safe-variables)) args))

(advice-add #'custom-set-variables :filter-args 'custom-set-only-safe)
(load custom-file)
(advice-remove #'custom-set-variables 'custom-set-only-safe)

(use-package no-littering)

;; * Appearance
;; ** User Interface
;; Some of these settings may also be set at the window-system level (e.g. XDefaults). Setting them
;; there will force them to be set before the frame starts, preventing an ephemeral "flickering" of
;; various UI artefacts. Regardless, they should also be set here for portability (XDefaults won't
;; apply to e.g. OSX).
(setg column-number-mode t)
(setg fringe-mode 0)
(setg inhibit-startup-message t)
(setg menu-bar-mode nil)
(setg scroll-bar-mode nil)
(setg tool-bar-mode nil)

;; ** Fonts
;; TODO: Implement font fall-back.
(let ((font-name "Iosevka-10"))
  (when (and (display-graphic-p) (find-font (font-spec :name font-name)))
    (set-frame-font font-name)))

;; ** Theme
(with-demoted-errors "%S"
  (load-theme 'skywave))

;; * Core Behaviors
;; ** Backups
(setg backup-by-copying t)
(setg delete-old-versions t)
(setg kept-new-versions 6)
(setg kept-old-versions 2)
(setg version-control t)

;; ** Editor
(setg fill-column 100)
(setg indent-tabs-mode nil)
(setg sentence-end-double-space nil)
(setg tab-width 2)

(defalias 'yes-or-no-p 'y-or-n-p)

;; * Minor Mode Initialization
(use-package general
  :defines general-prefix
           general-non-normal-prefix
           general-all-states
           general-command-states
           general-literal-states
  :config
  (defvar general-prefix "SPC")
  (defvar general-non-normal-prefix "M-SPC")
  (defvar general-all-states '(emacs insert motion normal visual))
  (defvar general-command-states '(motion normal visual))
  (defvar general-literal-states '(emacs insert)))

(use-package evil
  :init
  (evil-mode t)

  :config
  (setg evil-move-beyond-eol t)
  (setg evil-split-window-below t)
  (setg evil-vsplit-window-right t)

  :general (:states general-command-states
            "n" 'evil-backward-char
            "e" 'evil-next-visual-line
            "i" 'evil-previous-visual-line
            "o" 'evil-forward-char)

  :general (:states 'normal
            "h" 'evil-insert-state
            "H" 'evil-insert-line
            "y" 'evil-open-below
            "Y" 'evil-open-above

            "Q" 'evil-record-macro)

  :general (:states 'insert
            "RET" 'evil-ret-and-indent)

  ;; Unbinding doesn't work with `:states'.
  :general (:keymaps 'evil-normal-state-map
            "q" nil)

  :general (:keymaps 'evil-window-map
            "n" 'evil-window-left
            "e" 'evil-window-down
            "i" 'evil-window-up
            "o" 'evil-window-right

            "q" 'evil-window-delete)

  :general (:states general-all-states
            :prefix general-prefix
            :non-normal-prefix general-non-normal-prefix
            "w" 'evil-window-map))

(use-package outline
  :commands outline-hide-body
  :diminish outline-minor-mode

  :config
  ;; Use an actual ellipsis character.
  (set-display-table-slot
   standard-display-table
   'selective-display
   (string-to-vector "…")))

(use-package outshine
  :general (:states 'normal
            "<tab>" (general-predicate-dispatch (key-binding (kbd "<tab>"))
                      (and outline-minor-mode (outline-on-heading-p)) 'outline-cycle))
  :init
  (add-hook+ outline-minor-mode-hook/:outshine-initialization ()
    (outshine-hook-function)
    (font-lock-flush)
    (outline-hide-body))

  :config
  (defun wrap-in-save-excursion (fn args)
    (save-excursion (funcall fn args)))

  (advice-add #'outline-cycle :around 'wrap-in-save-excursion))

(provide 'init)
