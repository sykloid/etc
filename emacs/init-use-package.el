;; Emacs Initialization
;; P.C.Shyamshankar 'sykora' <sykora@lucentbeing.com>

;; This is rewrite #4.

;; Package System Initialization -- Must be done ahead of time.
(require 'package)

;; I gave up on melpa-stable, it's anything but.
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; Bootstrap use-package itself, if absent.
(unless (require 'use-package nil 'silent)
  (package-refresh-contents)
  (package-install 'use-package)
  (require 'use-package))

(defmacro setc (variable value)
  `(customize-set-variable ',variable ,value))

(use-package winner
  :init (winner-mode t))

(use-package undo-tree
  :diminish undo-tree-mode)

