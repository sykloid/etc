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

;; * Use-Package
(require 'use-package)

;; * Minor Modes
(use-package outshine
  :init
  (set-display-table-slot standard-display-table
                          'selective-display
                          (string-to-vector "…")))

;;; init.el ends here
