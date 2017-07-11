;; -*- eval: (outline-minor-mode) -*-
;; An Emacs Initialization
;; P.C. Shyamshankar
;; This is rewrite #5

;; * Meta Initialization
;; ** Utility Forms
;; *** Set Forms
;; The customization system is great for option discovery, but lousy for programmatic configuration.
;; Nevertheless, it has become the de-facto standard for specifying user-facing options, particuarly
;; using custom set logic. It's impossible to escape.
(defalias 'setl 'setq "Set a variable (buffer) locally.")
(defmacro setg (variable value)
  "Set `VARIABLE' to `VALUE' globally, respecting `custom-set' if necessary."
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

;; ** Package System
(eval-and-compile (require 'package))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(setl package-enable-at-startup nil)
(eval-and-compile (package-initialize))

;; ** Use-Package Bootstrap
;; `use-package' itself is only needed during byte-compilation.
(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setl use-package-always-ensure t))

;; ** Path Initialization
(eval-and-compile
  (defvar user-lisp-directory (concat user-emacs-directory "lisp/")))
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
           general-mode-prefix
           general-non-normal-prefix
           general-all-states
           general-command-states
           general-literal-states
  :config
  (defvar general-prefix "SPC")
  (defvar general-mode-prefix (concat general-prefix " " "m"))
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
            "o" 'evil-forward-char

            "N" 'evil-toggle-beginning-of-line
            "O" 'end-of-line

            "t" 'evil-search-next
            "T" 'evil-search-previous

            "k" 'evil-yank
            "m" 'evil-paste-after
            "M" 'evil-paste-before)

  :general (:states 'normal
            "a" 'evil-append
            "A" 'evil-append-line
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
            "w" 'evil-window-map)

  :init
  (defun evil-toggle-beginning-of-line ()
    (interactive)
    (let ((current (point)))
      (back-to-indentation)
      (when (= current (point))
        (beginning-of-line)))))

(use-package helm
  :general (:states general-all-states
            "M-x" 'helm-M-x)

  :general (:keymaps 'helm-map
            "C-e" 'helm-next-line
            "C-i" 'helm-previous-line
            "<tab>" 'helm-select-action)

  :init
  ;; Hide the cursor in the helm completion window.
  (add-hook+ helm-after-initialize-hook/:hide-helm-cursor ()
    (with-helm-buffer (setl cursor-in-non-selected-windows nil)))

  ;; Helm display configuration: show helm across the entirety of the window, regardless of splits.
  (setg helm-split-window-in-side-p t)
  (add-to-list
   'display-buffer-alist
   `(,(rx bos "*helm" (* not-newline) "*" eos)
     (display-buffer-in-side-window)
     (inhibit-same-window . t)
     (window-height . 0.4))))

(use-package helm-elisp
  :ensure nil
  :config
  (defun custom-group-p (sym)
    (or (and (get sym 'custom-loads) (not (get sym 'custom-autoload)))
        (get sym 'custom-group)))

  (defun helm-def-source--emacs-groups (&optional default)
    (helm-build-in-buffer-source "Groups"
      :init `(lambda () (helm-apropos-init #'custom-group-p ,default))
      :fuzzy-match helm-apropos-fuzzy-match
      :action '(("Customize Group" . (lambda (candidate) (customize-group (helm-symbolify candidate)))))))

  (add-to-list 'helm-apropos-function-list 'helm-def-source--emacs-groups t))

(use-package hydra
  :general (:states general-all-states
            :prefix general-prefix
            :non-normal-prefix general-non-normal-prefix
            "h" 'help-hydra/body)
  :init
  (defhydra help-hydra (:color blue :idle 1.0)
    ("a" helm-apropos)))

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

(use-package undo-tree
  :diminish undo-tree-mode
  :general (:states 'normal
            "u" 'undo-tree-undo
            "U" 'undo-tree-redo)

  :general (:keymaps 'undo-tree-visualizer-mode-map
            "n" 'undo-tree-visualize-switch-branch-left
            "e" 'undo-tree-visualize-redo
            "i" 'undo-tree-visualize-undo
            "o" 'undo-tree-visualize-switch-branch-right)

  :general (:states general-all-states
            :prefix general-prefix
            :non-normal-prefix general-non-normal-prefix
            "u" 'undo-tree-visualize)

  :init
  (evil-set-initial-state 'undo-tree-visualizer-mode 'emacs))

(use-package winner
  :ensure nil
  :general (:keymaps 'evil-window-map
            "u" 'winner-undo
            "U" 'winner-redo)
  :init
  (winner-mode))

(provide 'init)
