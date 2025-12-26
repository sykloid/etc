;;; init.el --- Emacs Configuration -*- lexical-binding: t -*-
;;; P.C. Shyamshankar 'sykloid' <shyam@sykloid.org>

;;; Commentary:
;;; This is the 7th rewrite of this configuration.

;;; In this version, we begin moving away from using nix to install
;;; emacs packages, leaning instead into native emacs tooling such as
;;; elpaca.

;;; Code:

;;; * Package Management & Bootstrap
(load-file (expand-file-name "elpaca-bootstrap.el" user-emacs-directory))
(setq elpaca-lock-file (expand-file-name "elpaca.lock.el" user-emacs-directory))

(setq use-package-enable-imenu-support t)
(require 'use-package)
(setq use-package-compute-statistics t)
(setq use-package-always-ensure t)
(elpaca elpaca-use-package (elpaca-use-package-mode))

;;; * Core Packages
(use-package no-littering :ensure (:wait t)
  :init
  (no-littering-theme-backups))

(use-package general :ensure (:wait t)
  :functions
  setg
  setd

  :init
  (defalias 'setd 'general-setq-default)
  (defalias 'setg 'general-setq)

  (defvar general-prefix-map (make-sparse-keymap))
  (general-define-key
   :states '(emacs insert normal motion visual)
   :prefix-map 'general-prefix-map
   :prefix "SPC"
   :non-normal-prefix "M-SPC")
  (general-create-definer with-prefix :keymaps 'general-prefix-map)

  (defvar utility-map (make-sparse-keymap))
  (general-define-key
   :states '(emacs normal motion visual)
   :prefix-map 'utility-map
   :prefix "s"
   :non-normal-prefix "M-s")
  (general-create-definer with-utility :keymaps 'utility-map))

(use-package emacs :ensure nil
  :general
  ("M-n" 'backward-sentence
   "M-e" 'forward-paragraph
   "M-i" 'backward-paragraph
   "M-o" 'forward-sentence)

  :config
  (defalias 'yes-or-no-p 'y-or-n-p)

  (setg custom-file (concat user-emacs-directory "custom.el"))
  (load custom-file t)
  (load-theme 'skywave)

  (prefer-coding-system       'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  (setg ad-redefinition-action 'accept)

  (setd indent-tabs-mode nil)
  (setd tab-width 2)
  (setg column-number-mode t)
  (setg frame-background-mode 'dark)
  (setg fringe-mode 0)
  (setg inhibit-startup-message t)
  (setg menu-bar-mode nil)
  (setg sentence-end-double-space nil)
  (setg show-trailing-whitespace t)
  (setg tool-bar-mode nil)

  (setg backup-by-copying t)
  (setg delete-old-versions t)
  (setg kept-new-versions 6)
  (setg kept-old-versions 2)
  (setg version-control t))

(use-package transient)

;;; * Keybinding and Movement
(use-package evil
  :init
  (evil-mode 1)

  (defun evil-select-last-pasted ()
    "Select the last pasted text."
    (interactive)
    (evil-goto-mark ?\[)
    (evil-visual-char)
    (evil-goto-mark ?\]))

  :config
  (setg evil-move-beyond-eol t)
  (setg evil-split-window-below t)
  (setg evil-vsplit-window-right t)

  (setd evil-shift-width 2)
  (setg evil-undo-system 'undo-fu)

  (add-to-list 'evil-emacs-state-modes 'minibuffer-mode)

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
   "M" 'evil-paste-before)

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

(use-package undo-fu
  :config
  (setg undo-fu-allow-undo-in-region t))

(use-package mwim
  :general
  (:states '(motion normal visual)
   "N" 'mwim-beginning
   "O" 'mwim-end))

(use-package evil-commentary
  :init
  (evil-commentary-mode)

  :general
  (with-prefix
    "c" 'evil-commentary-line
    "C" 'evil-commentary))

(use-package evil-surround
  :init
  (global-evil-surround-mode)

  :general
  (:states '(normal visual)
   "js" 'evil-surround-region))

(use-package xref :ensure nil
  :after evil
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
    "lo" 'xref-go-back
    "ln" 'xref-go-forward)

  (:keymaps 'xref--xref-buffer-mode-map
    "e" 'xref-next-line
    "E" 'xref-next-group
    "i" 'xref-prev-line
    "I" 'xref-prev-group
    "q" 'xref-quit-and-pop-marker-stack
    "RET" 'xref-quit-and-goto-xref))

(use-package which-key)

;;; * Completion
(use-package consult
  :general
  (with-prefix
    "/" 'consult-ripgrep
    "i" 'consult-imenu
    "o" 'consult-outline))

(use-package corfu
  :init
  (global-corfu-mode 1)

  :general
  (:keymaps 'corfu-map
   "M-e" 'corfu-next
   "M-i" 'corfu-previous)

  :config
  (setg corfu-auto t)
  (setg corfu-auto-delay 0.2)
  (setg corfu-quit-no-match 'separator))

(use-package corfu-terminal
  :ensure (:repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :init
  (corfu-terminal-mode 1))

(use-package vertico
  :init
  (vertico-mode)
  (vertico-buffer-mode)

  :config
  (setg vertico-buffer-display-action
  '(display-buffer-below-selected (window-height 13)))

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

(use-package orderless
  :config
  (setg completion-styles '(orderless basic)))

(use-package embark
  :after vertico which-key
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

(use-package embark-consult)

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode)

  :general
  (:keymaps 'vertico-map
    "M-," 'marginalia-cycle))

(use-package cape)

;;; * Static Analysis
(use-package eglot :ensure nil
  :config
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("rass" "python")))

  :general
  (with-prefix
    "la" 'eglot-code-actions
    "lf" 'eglot-format-buffer))

(use-package flymake :ensure nil
  :general
  (with-prefix
    "ll" 'consult-flymake
    "le" 'flymake-goto-next-error
    "li" 'flymake-goto-prev-error))

;;; * Source Control and Project Management
(use-package magit
  :mode ("COMMIT_EDITMSG" . git-commit-mode)
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

   "V" 'set-mark-command)
  (:keymaps '(magit-diff-mode-map magit-log-mode-map magit-status-mode-map)
   :prefix "SPC" :prefix-map 'general-prefix-map))

(use-package project :ensure nil
  :general
  (with-prefix
    "pf" 'project-find-file
    "pb" 'consult-project-buffer
    "pi" 'consult-imenu-multi))

(use-package breadcrumb
  :init
  (breadcrumb-mode))

(use-package tempel
  :general
  (:keymaps 'tempel-map :states '(insert)
   "M-n" 'tempel-previous
   "M-o" 'tempel-next
   "TAB" 'tempel-next)

  (:states '(insert)
   "M-+" 'tempel-insert)

  (with-utility
    "t" 'tempel-insert))

;;; * Miscellaneous Minor Modes
(use-package backline
  :after outline
  :init (advice-add 'outline-flag-region :after 'backline-update))

(use-package outline :ensure nil
  :init
  (outline-minor-mode)

  :config
  (set-display-table-slot standard-display-table
                          'selective-display
                          (string-to-vector "…")))

(use-package outline-minor-faces
  :after outline
  :init
  (general-add-hook 'outline-minor-mode-hook #'outline-minor-faces-mode))

(use-package winner :ensure nil
  :init (winner-mode 1)
  :general
  (:keymaps 'evil-window-map
   "u" 'winner-undo
   "U" 'winner-redo))
  

;;; * File-Type Major Modes
(use-package elisp-mode :ensure nil
  :config
  ;; From: https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94
  (defun fuco1/lisp-indent-function (indent-point state)
    "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
    (let ((normal-indent (current-column))
          (orig-point (point)))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond
        ;; car of form doesn't seem to be a symbol, or is a keyword
        ((and (elt state 2)
              (or (not (looking-at "\\sw\\|\\s_"))
                  (looking-at ":")))
         (if (not (> (save-excursion (forward-line 1) (point))
                     calculate-lisp-indent-last-sexp))
             (progn (goto-char calculate-lisp-indent-last-sexp)
                    (beginning-of-line)
                    (parse-partial-sexp (point)
                                        calculate-lisp-indent-last-sexp 0 t)))
         ;; Indent under the list or under the first sexp on the same
         ;; line as calculate-lisp-indent-last-sexp.  Note that first
         ;; thing on that line has to be complete sexp since we are
         ;; inside the innermost containing sexp.
         (backward-prefix-chars)
         (current-column))
        ((and (save-excursion
                (goto-char indent-point)
                (skip-syntax-forward " ")
                (not (looking-at ":")))
              (save-excursion
                (goto-char orig-point)
                (looking-at ":")))
         (save-excursion
           (goto-char (+ 2 (elt state 1)))
           (current-column)))
        (t
         (let ((function (buffer-substring (point)
                                           (progn (forward-sexp 1) (point))))
               method)
           (setq method (or (function-get (intern-soft function)
                                          'lisp-indent-function)
                            (get (intern-soft function) 'lisp-indent-hook)))
           (cond ((or (eq method 'defun)
                      (and (null method)
                           (> (length function) 3)
                           (string-match "\\`def" function)))
                  (lisp-indent-defform state indent-point))
                 ((integerp method)
                  (lisp-indent-specform method state
                                        indent-point normal-indent))
                 (method
                  (funcall method indent-point state))))))))

  (general-add-hook 'emacs-lisp-mode-hook
                    (lambda () (setq-local lisp-indent-function #'fuco1/lisp-indent-function)))

  (defun emacs-lisp-set-outline-regexp ()
    (setg outline-regexp "\s*;;; \\*+ "))

  (general-add-hook 'emacs-lisp-mode-hook
                    '(emacs-lisp-set-outline-regexp outline-minor-mode)))

(use-package python :ensure nil
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

(use-package jsonnet-mode
  :after eglot
  :init
  (add-to-list 'eglot-server-programs '(jsonnet-mode . ("jsonnet-language-server" "-t"))))

(use-package nix-mode)
(use-package yaml-mode)

(provide 'init)
;;; init.el ends here
