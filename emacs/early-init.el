;;; early-init.el --- Emacs Early Initialization

;;; Commentary:
;;; This file is loaded before the package management infrastructure and the UI,
;;; should contain (and only contain) the customizations relevant to those
;;; subsystems.

;;; Code:

(setq inhibit-startup-screen t)
(menu-bar-mode -1)

;; Package Management
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
