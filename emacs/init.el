;;; init.el --- Emacs Configuration -*- eval: (outshine-mode) -*-
;;; P.C. Shyamshankar 'sykloid' <shyam@sykloid.org>

;;; Commentary:
;; This is the 6th rewrite of this configuration.

;;; Code:
;; * Package System Initialization
;; One of the novelties of this rewrite is the movement of all package
;; installation to ~airlift~.
(setq package-archives nil)
(setq package-enable-at-startup nil)
(package-initialize)

;; * Use-Package
(require 'use-package)

;; * Minor Modes
(use-package outshine
  :init
  (set-display-table-slot standard-display-table
                          'selective-display
                          (string-to-vector "…")))

;;; init.el ends here
