;; init.el --- Emacs Configuration
;; P.C. Shyamshankar 'sykloid' <shyam@sykloid.org>

;; Commentary:
;; This is the 6th rewrite of this configuration.

;; Code:
;; * Package System Initialization
;; One of the novelties of this rewrite is the movement of all package
;; installation to ~airlift~.
(setq package-archives nil)
(setq package-enable-at-startup nil)
(package-initialize)

;; init.el ends here
