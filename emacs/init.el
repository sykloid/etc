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
(setq use-package-enable-imenu-support t)
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
    :prefix general-utility-prefix))

;; * No Littering
(use-package no-littering
  :config
  (no-littering-theme-backups))

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

  (setg show-trailing-whitespace t)

  (use-package autorevert :diminish auto-revert-mode)
  (use-package eldoc :diminish eldoc-mode)

  (defun terminal-title-update ()
    (interactive)
    (send-string-to-terminal (concat "\033]2; " (buffer-name) "\007"))
    (if buffer-file-name
        (send-string-to-terminal (concat "\033]2; " (buffer-file-name) "\007"))
      (send-string-to-terminal (concat "\033]2; " (buffer-name) "\007"))))

  (add-hook 'post-command-hook 'terminal-title-update))

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

  (add-to-list 'evil-emacs-state-modes 'minibuffer-mode)

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

  (with-prefix
    "bf" 'find-file
    "bb" 'switch-to-buffer
    "bk" 'kill-buffer)

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
    "Apply ANSI color codes to the compilation buffer."
    (ansi-color-apply-on-region compilation-filter-start (point)))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

  (setg compilation-scroll-output t))

(use-package elisp-mode
  :ensure nil
  :config
  (defun emacs-lisp-set-outline-regexp ()
    (setg outline-regexp "\s*;; \\*+ "))
  (general-add-hook 'emacs-lisp-mode-hook
                    '(emacs-lisp-set-outline-regexp
                      outline-minor-mode)))

;; * Minor Modes
(use-package company
  :diminish company-mode
  :init
  (global-company-mode 1)

  :general
  (:keymaps 'company-mode-map
   "M-e" 'company-select-next
   "M-i" 'company-select-previous))

(use-package consult
  :general
  (with-prefix
    "/" 'consult-ripgrep
    "i" 'consult-imenu))

(use-package dired
  :general
  (:keymaps 'dired-mode-map
    "n" 'dired-up-directory
    "e" 'dired-next-line
    "i" 'dired-previous-line
    "o" 'dired-find-file
    "SPC" 'general-prefix-map)

  :init
  (add-hook 'dired-mode-hook #'hl-line-mode)

  :config
  (setg dired-kill-when-opening-new-dired-buffer t))

(use-package direnv
  :config
  (direnv-mode))

(use-package eglot
  :general
  (with-prefix
    "la" 'eglot-code-actions
    "ln" 'eglot-rename
    "lt" 'eglot-find-typeDefinition))

(use-package embark
  :after vertico
  :init
  ;; From: https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)
  :general
  (:keymaps 'vertico-map
    "M-." 'embark-act))

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

(use-package expand-region
  :general
  (:keymaps 'evil-visual-state-map
    "." 'er/expand-region))

(use-package flymake
  :general
  (with-prefix
    "ll" 'consult-flymake
    "le" 'flymake-goto-next-error
    "li" 'flymake-goto-prev-error))

(use-package forge
  :after magit)

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

  (:keymaps '(magit-diff-mode-map magit-log-mode-map magit-status-mode-map)
   "n" 'backward-char
   "e" 'magit-next-line
   "i" 'magit-previous-line
   "o" 'forward-char

   "V" 'set-mark-command
   "SPC" 'general-prefix-map))

(use-package magit-todos
  :init
  (magit-todos-mode))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode)
  :general
  (:keymaps 'vertico-map
    "M-," 'marginalia-cycle))

(use-package mwim
  :general
  (:states '(motion normal visual)
   "N" 'mwim-beginning
   "O" 'mwim-end))

(use-package outline
  :config
  (set-display-table-slot standard-display-table
                          'selective-display
                          (string-to-vector "…")))

(use-package outline-minor-faces
  :after outline
  :init
  (general-add-hook 'outline-minor-mode-hook #'outline-minor-faces-mode))

(use-package backline
  :after outline
  :init (advice-add 'outline-flag-region :after 'backline-update))

(use-package bicycle
  :after outline
  :general
  (:states '(normal)
   "zc" 'bicycle-cycle
   "zC" 'bicycle-cycle-global))

(use-package orderless
  :config
  (setg completion-styles '(orderless basic)))

(use-package project
  :general
  (with-prefix
    "pf" 'project-find-file
    "pb" 'consult-project-buffer
    "pi" 'consult-imenu-multi))

(use-package rainbow-mode
  :hook (emacs-lisp-mode))

(use-package vertico
  :init
  (vertico-mode)
  (vertico-buffer-mode)

  :config
  (setg vertico-buffer-display-action
  '(display-buffer-below-selected (window-height 13))))

(use-package vertico-directory
  :after vertico
  :general
  (:keymaps 'vertico-map
    "M-e" 'vertico-next
    "M-i" 'vertico-previous

    "M-E" 'vertico-next-group
    "M-I" 'vertico-previous-group

    "RET" 'vertico-directory-enter
    "DEL" 'vertico-directory-delete-char
    "M-DEL" 'vertico-directory-delete-word)
  :hook (rfn-shadow-update-overlay . vertico-directory-tidy))

(use-package xref
  :init
  (add-to-list 'evil-emacs-state-modes 'xref--xref-buffer-mode)
  (add-hook 'xref--xref-buffer-mode-hook 'hl-line-mode)

  (setg xref-show-xrefs-function #'consult-xref)
  (setg xref-show-definitions-function #'consult-xref)

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
;;; End:
;;; init.el ends here
