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

;; * Decorations
(menu-bar-mode -1)

;; * Use-Package
(require 'use-package)

;; * General
(use-package general
  :defines
  general-prefix
  general-utility-prefix

  :functions
  with-prefix
  with-utility

  :config
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

;; * Evil
(use-package evil
  :init
  (evil-mode 1)

  :config
  (general-setq evil-move-beyond-eol t)
  (general-setq evil-split-window-below t)
  (general-setq evil-vsplit-window-right t)
  :general
  (:states 'normal
   "a" 'evil-append
   "A" 'evil-append-line
   "h" 'evil-insert-state
   "H" 'evil-insert-line
   "y" 'evil-open-below
   "Y" 'evil-open-above
   "Q" 'evil-record-macro)

  (:states '(motion normal visual)
   "n" 'evil-backward-char
   "e" 'evil-next-visual-line
   "i" 'evil-previous-visual-line
   "o" 'evil-forward-char

   "N" 'evil-beginning-of-line
   "O" 'evil-end-of-line)

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
   "q" 'evil-window-delete))

;; * Minor Modes
(use-package outshine
  :init
  (set-display-table-slot standard-display-table
                          'selective-display
                          (string-to-vector "…")))

(use-package magit
  :init
  (evil-set-initial-state 'magit-status 'emacs)
  :general
  (with-prefix
   "vs" 'magit-status)
  (:keymaps '(magit-diff-mode-map magit-status-mode-map)
   "n" 'backward-char
   "e" 'magit-next-line
   "i" 'magit-previous-line
   "o" 'forward-char

   "V" 'set-mark-command))

;;; Local Variables:
;;; eval: (outshine-mode)
;;; End:
;;; init.el ends here
