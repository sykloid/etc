;; -*- eval: (outline-minor-mode); flycheck-disabled-checkers: (emacs-lisp-checkdoc) -*-
;; An Emacs Initialization
;; P.C. Shyamshankar
;; This is rewrite #5

;; * Meta Initialization
;; ** Utility Forms
;; *** Set Forms
;; The customization system is great for option discovery, but lousy for programmatic configuration.
;; Nevertheless, it has become the de-facto standard for specifying user-facing options, particuarly
;; using custom set logic. It's impossible to escape.
(eval-and-compile
  (defalias 'setl 'setq "Set a variable (buffer) locally.")
  (defmacro setg (variable value)
    "Set `VARIABLE' to `VALUE' globally, respecting `custom-set' if necessary."
    `(customize-set-variable ',variable ,value)))

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
  (defvar user-lisp-directory (concat user-emacs-directory "lisp/"))
  (add-to-list 'load-path user-lisp-directory))
(setg custom-theme-directory (concat user-lisp-directory "themes/"))

;; ** Host-Specific Initialization
(load-library "init-host")
(with-demoted-errors "%S" (load-library (format "init-%s" (system-name))))

;; ** Custom Initialization
(setg custom-file (concat user-emacs-directory "customizations.el"))

(defvar custom-safe-variables
  '(custom-safe-themes))

(defun custom-set-only-safe (args)
  (cl-remove-if-not (lambda (elt) (member (car elt) custom-safe-variables)) args))

(advice-add #'custom-set-variables :filter-args 'custom-set-only-safe)
(load custom-file 'noerror)
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
;; TODO: Handle font fallback.
(let ((preferred-font "Iosevka-10"))
  (setg default-frame-alist (list (font . preferred-font))))

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
(setg scroll-step 1)
(setg sentence-end-double-space nil)
(setg tab-width 2)

;; ** Miscellaneous
(defalias 'yes-or-no-p 'y-or-n-p)
(setg ad-redefinition-action 'accept)

;; * Minor Mode Initialization
;; ** General Keybinding
(use-package general
  :defines
  general-prefix
  general-mode-prefix
  general-non-normal-prefix
  general-prefix-map

  :functions
  with-prefix
  with-mode-prefix

  :config
  (defvar general-prefix "SPC")
  (defvar general-non-normal-prefix "M-SPC")

  (general-create-definer
   with-prefix
   :states '(emacs insert motion normal visual)
   :prefix general-prefix
   :non-normal-prefix general-non-normal-prefix
   :prefix-command 'general-prefix-map)

  (defvar general-mode-sub-prefix "m")

  (general-create-definer
   with-mode-prefix
   :states '(emacs insert motion normal visual)
   :prefix (concat general-prefix " " general-mode-sub-prefix)
   :non-normal-prefix (concat general-non-normal-prefix " " general-mode-sub-prefix)))

(use-package which-key
  :diminish 'which-key-mode
  :init
  (setg which-key-allow-evil-operators t)
  (setg which-key-show-operator-state-maps t)
  (which-key-mode))

;; ** Evil
(use-package evil
  :init
  (evil-mode t)

  :config
  (setg evil-move-beyond-eol t)
  (setg evil-split-window-below t)
  (setg evil-vsplit-window-right t)

  :general
  (:states '(motion normal visual)
   "n" 'evil-backward-char
   "e" 'evil-next-visual-line
   "i" 'evil-previous-visual-line
   "o" 'evil-forward-char

   "N" 'evil-toggle-beginning-of-line
   "O" 'end-of-line

   "t" 'evil-search-next
   "T" 'evil-search-previous

   "k" 'evil-yank)

  :general
  (:states '(normal visual)
   "m" 'evil-paste-after
   "M" 'evil-paste-before)

  :general
  (:states 'normal
   "a" 'evil-append
   "A" 'evil-append-line
   "h" 'evil-insert-state
   "H" 'evil-insert-line
   "y" 'evil-open-below
   "Y" 'evil-open-above

   "Q" 'evil-record-macro)

  :general
  (:states '(operator visual)
   "h" '(:keymap evil-inner-text-objects-map))

  :general
  (:states 'insert
   "RET" 'evil-ret-and-indent)

  ;; Unbinding doesn't work with `:states'.
  :general (:keymaps 'evil-normal-state-map "q" nil)

  :general
  (:keymaps 'evil-window-map
   "n" 'evil-window-left
   "e" 'evil-window-down
   "i" 'evil-window-up
   "o" 'evil-window-right

   "d" 'delete-other-windows
   "q" 'evil-window-delete)

  :general (with-prefix "w" 'evil-window-map)

  :init
  (defun evil-toggle-beginning-of-line ()
    (interactive)
    (let ((current (point)))
      (back-to-indentation)
      (when (= current (point))
        (beginning-of-line)))))

(use-package evil-surround
  :ensure t
  :general (:states '(normal visual)
            "js" 'evil-surround-region
            "jS" 'evil-Surround-region)
  :init
  (global-evil-surround-mode))

(use-package comment-dwim-toggle
  :ensure nil
  :load-path user-lisp-directory
  :general (with-prefix "c" 'comment-dwim-toggle))

(use-package helm
  :general ("M-x" 'helm-M-x)

  :general
  (with-prefix
   "b" '(nil :which-key "Buffer Commands")
   "bb" 'helm-buffers-list
   "bf" 'helm-find-files
   "bk" 'kill-this-buffer
   "bK" 'kill-this-buffer-and-file
   "bo" 'evil-buffer
   "bR" 'rename-this-buffer-and-file)

  :general
  (:keymaps 'helm-map
   "M-e" 'helm-next-line
   "M-i" 'helm-previous-line
   "M-E" 'helm-next-source
   "M-I" 'helm-previous-source
   "TAB" 'helm-select-action)

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
     (window-height . 0.4)))

  (defun kill-this-buffer-and-file ()
    "Kill the current buffer and deletes the file it is visiting."
    (interactive)
    (let ((filename (buffer-file-name)))
      (when filename
        (if (vc-backend filename)
            (vc-delete-file filename)
          (progn
            (delete-file filename)
            (message "Deleted file %s" filename)
            (kill-buffer))))))

  (defun rename-this-buffer-and-file (new-location)
    "Rename the current buffer, and the file it is visiting."
    (interactive "FNew location: ")
    (let ((name (buffer-name))
          (file-name (buffer-file-name)))
      (if (not file-name)
          (message "%s isn't visiting a file!" name)
        (if (get-buffer new-location)
            (message "A buffer named '%s' already exists!" new-location)
          (rename-file file-name new-location 1)
          (rename-buffer new-location)
          (set-visited-file-name new-location)
          (set-buffer-modified-p nil))))))

(use-package helm-elisp
  :ensure helm
  :general (with-prefix "ha" 'helm-apropos)
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

(use-package helm-imenu
  :ensure helm
  :config
  (add-to-list 'helm-imenu-type-faces '("^Sections$" . outline-1)))

(use-package flycheck
  :general
  (with-prefix
   "f" '(nil :which-key "Flycheck Commands")
   "fb" 'flycheck-buffer
   "fe" 'flycheck-next-error
   "fi" 'flycheck-previous-error
   "fl" 'flycheck-list-errors)

  :init
  (add-hook+ prog-mode-hook/:enable-flycheck ()
    (global-flycheck-mode)))

(use-package outline
  :commands outline-hide-body
  :diminish outline-minor-mode
  :general
  (with-mode-prefix :keymaps 'outline-minor-mode-map
    "i" 'helm-semantic-or-imenu)

  :config
  ;; Use an actual ellipsis character.
  (set-display-table-slot
   standard-display-table
   'selective-display
   (string-to-vector "…")))

(use-package outshine
  :general
  (:states 'normal
   "TAB" (general-predicate-dispatch (key-binding (kbd "TAB"))
           (and outline-minor-mode (outline-on-heading-p)) #'outline-cycle)
   "<backtab>" (general-predicate-dispatch (key-binding (kbd "<backtab>"))
                 outline-minor-mode #'outshine-cycle-buffer))
  :init
  (setg outshine-imenu-show-headlines-p nil)
  (add-hook+ outline-minor-mode-hook/:outshine-initialization ()
    (outshine-hook-function)
    (font-lock-flush)
    (outline-hide-body)
    (reveal-mode)
    (add-to-list 'imenu-generic-expression '("Sections" "^;; [*]+ \\(.*\\)" 1)))

  :config
  (defun wrap-in-save-excursion (fn args)
    (save-excursion (funcall fn args)))

  (advice-add #'outline-cycle :around 'wrap-in-save-excursion))

(use-package projectile
  (with-prefix
   "p" '(nil :which-key "Project Commands"))

(use-package helm-projectile
  :general
  (with-prefix
   "pp" 'helm-projectile))

(use-package undo-tree
  :diminish undo-tree-mode
  :general
  (:states 'normal
   "u" 'undo-tree-undo
   "U" 'undo-tree-redo)

  :general
  (:keymaps 'undo-tree-visualizer-mode-map
   "n" 'undo-tree-visualize-switch-branch-left
   "e" 'undo-tree-visualize-redo
   "i" 'undo-tree-visualize-undo
   "o" 'undo-tree-visualize-switch-branch-right)

  :general (with-prefix "u" 'undo-tree-visualize)

  :init
  (evil-set-initial-state 'undo-tree-visualizer-mode 'emacs))

(use-package winner
  :ensure nil
  :general
  (:keymaps 'evil-window-map
   "u" 'winner-undo
   "U" 'winner-redo)
  :init
  (winner-mode))

;; ** Magit
(use-package magit
  :general
  (:keymaps 'magit-mode-map
   "n" 'evil-backward-char
   "e" 'evil-next-line
   "i" 'evil-previous-line
   "o" 'evil-forward-char

   "V" 'set-mark-command

   "M-n" 'magit-section-up

   general-prefix general-prefix-map)

  :init
  (evil-set-initial-state 'magit-status 'emacs))

;; ** VC
(use-package vc
  :ensure nil
  ;; TODO: Make this binding more generic, using vc-dir for non-git backends.
  :general (with-prefix "vs" 'magit-status))

;; * Major Modes
;; ** Ebib
(use-package ebib
  :general
  (:keymaps 'ebib-index-mode-map
   "e" 'ebib-next-entry
   "i" 'ebib-prev-entry
   general-prefix general-prefix-map)

  :general
  (:keymaps 'ebib-entry-mode-map
   "e" 'ebib-next-field
   "i" 'ebib-prev-field
   general-prefix general-prefix-map)

  :config
  (setg ebib-bib-search-dirs host-ebib-bib-search-dirs)
  (setg ebib-file-search-dirs host-ebib-file-search-dirs)
  (setg ebib-preload-bib-files host-ebib-preload-bib-files)
  (setg ebib-file-associations host-ebib-file-associations)

  (setg ebib-layout 'index-only)

  (evil-set-initial-state 'ebib-entry-mode 'emacs)
  (evil-set-initial-state 'ebib-index-mode 'emacs))

;; ** Haskell
(use-package haskell-mode
  :mode ("\\.hs'" . haskell-mode)
  :config
  (add-hook+ haskell-mode-hook/:haskell-minor-modes ()
    (haskell-decl-scan-mode)
    (haskell-indentation-mode)))

;; ** Lisp
(use-package lisp-mode
  :ensure nil
  :mode ("\\.el'" . emacs-lisp-mode)

  :general
  (with-mode-prefix :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
   "e" '(nil :which-key "Evaluation")
   "ex" 'eval-last-sexp
   "eX" 'eval-and-replace-last-sexp
   "ef" 'eval-defun
   "eb" 'eval-buffer)

  :config
  (defun eval-and-replace-last-sexp ()
    (interactive)
    (condition-case nil
        (progn
          (let ((result (eval (preceding-sexp))))
            (backward-kill-sexp)
            (prin1 result (current-buffer))))
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
      (error (message "Invalid SExp")
             (insert (current-kill 0))))))

;; ** OCaml
(use-package tuareg
  :mode ("\\.mli?'" . tuareg-mode))

(use-package merlin
  :general
  (with-mode-prefix :keymaps 'tuareg-mode-map
    "t" #'merlin-type-enclosing)

  :init
  (setg merlin-error-after-save nil)
  (add-hook+ tuareg-mode-hook/:initialize-merlin ()
    (merlin-mode))

  :config
  (use-package flycheck-ocaml
    :init
    (flycheck-ocaml-setup)))

;; ** Org
(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org'" . org-mode)
  :general
  (:keymaps 'org-mode-map
   "M-n" 'org-metaleft
   "M-e" 'org-metadown
   "M-i" 'org-metaup
   "M-o" 'org-metaright

   "M-N" 'org-shiftmetaleft
   "M-E" 'org-shiftmetadown
   "M-I" 'org-shiftmetaup
   "M-O" 'org-shiftmetaright

   "M-RET" 'org-metareturn)

  :general
  (with-prefix
   "o" '(nil :which-key "Org")
   "oc" 'org-capture)

  :config
  (setg org-adapt-indent nil))

(use-package org-agenda
  :ensure org-plus-contrib
  :general
  (with-prefix
   "oa" 'org-agenda)

  :general
  (:keymaps 'org-agenda-mode-map
   "e" 'org-agenda-next-item
   "i" 'org-agenda-previous-item)

  :init
  (use-package hl-line
    :init
    (add-hook+ org-agenda-mode-hook/:enable-hl-line-mode ()
      (hl-line-mode))))

  :config
  (setg org-agenda-files host-org-agenda-directory))

(use-package org-capture
  :ensure org-plus-contrib
  :config
  (setg org-capture-templates
        `(("t" "Triage" entry (file host-org-capture-triage-path)
           "* TODO %^{Title}" :kill-buffer t :prepend t :immediate-finish t))))

;; * Exeunt
(provide 'init)
