;;; init.el --- Emacs Configuration -*- eval: (outshine-mode) -*-
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

;; * Minor Modes
(use-package outshine
  :init
  (set-display-table-slot standard-display-table
                          'selective-display
                          (string-to-vector "…")))

;;; init.el ends here
