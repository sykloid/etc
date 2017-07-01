;; An Emacs Initialization
;; P.C. Shyamshankar
;; This is rewrite #5

;; * Meta Initialization
;; ** Package System
(eval-and-compile (require 'package))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(setq package-enable-at-startup nil)
(eval-and-compile (package-initialize))
