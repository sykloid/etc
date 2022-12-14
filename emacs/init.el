;;; -*- lexical-binding: t -*-
;;; init.el --- Emacs Configuration
;;; P.C. Shyamshankar 'sykloid' <shyam@sykloid.org>

;;; Commentary:
;; This is the 6th rewrite of this configuration.

;;; Code:
;; * Package System Initialization
;; We choose not to use emacs' built-in package manager, instead all package
;; management is done through ~nix~.
(require 'package)
(setq package-archives nil)
(setq package-enable-at-startup nil)
(package-initialize)

(load-theme 'skywave t)

;; * Use-Package
(require 'use-package)

;; * General
(use-package general
  :defines
  general-prefix
  general-utility-prefix

  :functions
  setg
  setgd

  with-prefix
  with-utility

  :config
  (defalias 'setg 'general-setq)
  (defalias 'setgd 'general-setq-default)

  (defvar general-prefix "SPC")
  (general-create-definer
    with-prefix
    :states '(motion normal visual)
    :prefix general-prefix
    :prefix-command 'general-prefix-map)

  (defvar general-utility-prefix "s")
  (general-create-definer
    with-utility
    :states '(normal visual)
    :prefix general-utility-prefix)

  ;; TODO: Make this more modular.
  (general-after-init
    (general-define-key :states 'normal
      "TAB" (general-predicate-dispatch nil
              (and outshine-mode (looking-at outline-regexp)) 'outshine-cycle)
      "S-TAB" (general-predicate-dispatch nil
                outshine-mode 'outshine-cycle-buffer))))

;; * No Littering
(use-package no-littering
  :config
  (setg auto-save-file-name-transforms
                `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; * Appearance, Decorations, and Editor-Wide Configuration
(use-package emacs
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)

  (setg custom-file (concat user-emacs-directory "custom.el"))

  (prefer-coding-system       'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setg default-buffer-file-coding-system 'utf-8)

  (setg ad-redefinition-action 'accept)

  (setg column-number-mode t)
  (setg frame-background-mode 'dark)
  (setg fringe-mode 0)
  (setgd indent-tabs-mode nil)
  (setg inhibit-startup-message t)
  (setg menu-bar-mode nil)
  (setg sentence-end-double-space nil)
  (setgd tab-width 2)
  (setg tool-bar-mode nil)

  (setg backup-by-copying t)
  (setg delete-old-versions t)
  (setg kept-new-versions 6)
  (setg kept-old-versions 2)
  (setg version-control t)

  (use-package autorevert :diminish auto-revert-mode)
  (use-package eldoc :diminish eldoc-mode))

;; * Evil
(use-package evil
  :init
  (evil-mode 1)

  :config
  (setg evil-move-beyond-eol t)
  (setg evil-split-window-below t)
  (setg evil-vsplit-window-right t)

  (setgd evil-shift-width 2)

  (setg evil-undo-system 'undo-fu)

  :init
  (defun evil-select-last-pasted ()
    "Select the last pasted text."
    (interactive)
    (evil-goto-mark ?\[)
    (evil-visual-char)
    (evil-goto-mark ?\]))

  :general
  (:states 'normal
   "a" 'evil-append
   "A" 'evil-append-line
   "h" 'evil-insert-state
   "H" 'evil-insert-line
   "y" 'evil-open-below
   "Y" 'evil-open-above

   "u" 'evil-undo
   "U" 'evil-redo)

  (:states '(normal motion)
   "Q" 'evil-record-macro

   "t" 'evil-search-next
   "T" 'evil-search-previous)

  (:states '(motion normal visual)
   "n" 'evil-backward-char
   "e" 'evil-next-visual-line
   "i" 'evil-previous-visual-line
   "o" 'evil-forward-char

   "N" 'evil-beginning-of-line
   "O" 'evil-end-of-line

   "k" 'evil-yank)

  (:states '(normal visual)
   "m" 'evil-paste-after
   "M" 'evil-paste-before
   "s" nil)

  (:states '(operator visual)
   "h" '(:keymap evil-inner-text-objects-map))

  (:keymaps 'evil-normal-state-map "q" nil)

  (:keymaps 'evil-window-map
   "n" 'evil-window-left
   "e" 'evil-window-down
   "i" 'evil-window-up
   "o" 'evil-window-right

   "d" 'delete-other-windows
   "q" 'evil-window-delete)

  (with-prefix
    "w" 'evil-window-map)

  (with-utility
    "v" 'evil-select-last-pasted))

(use-package evil-surround
  :init
  (global-evil-surround-mode)
  :general
  (:states '(normal visual)
   "js" 'evil-surround-region))

;; * Major Modes
(use-package compile
  :init
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point)))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

  (setg compilation-scroll-output t))

;; * Minor Modes
(use-package company
  :diminish company-mode
  :init
  (global-company-mode 1)

  :general
  (:keymaps 'company-mode-map
   "M-e" 'company-select-next
   "M-i" 'company-select-previous))

(use-package direnv
  :config
  (direnv-mode))

(use-package eglot
  :general
  (with-prefix
    "la" 'eglot-code-actions
    "ln" 'eglot-rename
    "lt" 'eglot-find-typeDefinition))

(use-package evil-args
  :general
  (:keymaps 'evil-inner-text-objects-map
   "," 'evil-inner-arg)
  (:keymaps 'evil-outer-text-objects-map
   "," 'evil-outer-arg)
  (:keymaps 'evil-motion-state-map
   "]," 'evil-forward-arg
   "[," 'evil-backward-arg))

(use-package evil-commentary
  :init
  (evil-commentary-mode)
  :general
  (with-prefix
    "c" 'evil-commentary-line
    "C" 'evil-commentary))

(use-package evil-exchange
  :general
  (with-utility
    "x" 'evil-exchange
    "X" 'evil-exchange-cancel))

(use-package flymake
  :general
  (with-prefix
    "le" 'flymake-goto-next-error
    "li" 'flymake-goto-prev-error))

(use-package helm
  :init
  (setg helm-split-window-in-side-p t)

  (defun helm-find-files-del-dwim ()
    "Do the right thing when pressing backspace during `helm-find-files'.

If looking back at a directory, delete the last path component.
Otherwise, delete a single character."
    (interactive)
    (if (looking-back "/" 1)
        (call-interactively 'helm-find-files-up-one-level)
      (delete-backward-char 1)))

  :general
  ("M-x" 'helm-M-x)

  (:keymaps 'helm-map
   "TAB" 'helm-execute-persistent-action
   "C-j" 'helm-select-action
   "M-e" 'helm-next-line
   "M-i" 'helm-previous-line
   "M-E" 'helm-previous-source
   "M-I" 'helm-next-source)

  (:keymaps 'helm-find-files-map
   "DEL" 'helm-find-files-del-dwim
   "M-e" 'helm-next-line
   "M-i" 'helm-previous-line)

  (with-prefix
    "bf" 'helm-find-files
    "li" 'helm-imenu))

(use-package helm-projectile
  :general
  (with-prefix "pf" 'helm-projectile))

(use-package helm-rg
  :general
  (with-prefix "p/" 'helm-projectile-rg))

(use-package hl-todo
  :init
  (global-hl-todo-mode))

(use-package magit
  :init
  (evil-set-initial-state 'magit-status 'emacs)

  :general
  (with-prefix
    "vb" 'magit-blame
    "vs" 'magit-status)

  (:keymaps '(magit-diff-mode-map magit-status-mode-map)
   "n" 'backward-char
   "e" 'magit-next-line
   "i" 'magit-previous-line
   "o" 'forward-char

   "V" 'set-mark-command))

(use-package magit-todos
  :init
  (magit-todos-mode))

(use-package mwim
  :general
  (:states '(motion normal visual)
   "N" 'mwim-beginning
   "O" 'mwim-end))

(use-package outshine
  :diminish outshine-mode outline-minor-mode
  :init
  (set-display-table-slot standard-display-table
                          'selective-display
                          (string-to-vector "…")))

(use-package xref
  :init
  (add-to-list 'evil-emacs-state-modes 'xref--xref-buffer-mode)
  (add-hook 'xref--xref-buffer-mode-hook 'hl-line-mode)

  :general
  (with-prefix
    "ld" 'xref-find-definitions
    "lD" 'xref-find-definitions-other-window
    "lr" 'xref-find-references
    "lo" 'xref-pop-marker-stack)

  (:keymaps 'xref--xref-buffer-mode-map
    "e" 'xref-next-line
    "E" 'xref-next-group
    "i" 'xref-prev-line
    "I" 'xref-prev-group
    "q" 'xref-quit-and-pop-marker-stack
    "RET" 'xref-quit-and-goto-xref))

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode 1)

  :config
  (setg which-key-allow-evil-operators t)
  (setg which-key-show-operator-state-maps t))

(use-package winner
  :init (winner-mode 1)
  :general (:keymaps 'evil-window-map
            "u" 'winner-undo
            "U" 'winner-redo))

;; * Exeunt
;;; Local Variables:
;;; eval: (outshine-mode)
;;; End:
;;; init.el ends here
