; Interface Initialization

;; Theming
(defvar user-theme-directory
  (concat mode-agnostic-init-directory "themes/")
  "Location of user theme files.")
(add-to-list 'custom-theme-load-path user-theme-directory)

(load-theme 'skywave t)

;; Window Decoration

(menu-bar-mode -1)
(set-default 'show-trailing-whitespace t)

;; Scrolling
(set 'scroll-step 1)

;; Buffer Widths
(set-default 'fill-column 100)
(set-default 'sentence-end-double-space nil)

;; Tabs
(set-default 'indent-tabs-mode nil)
(set-default 'tab-width 4)
(set-default 'indent-line-function 'insert-tab)

;; Evil Initialization
(evil-mode 1)
(set 'evil-esc-delay 0.001)

(require 'init-keymap)

;; Auto-Completion Initialization

(require 'auto-complete-config)
(ac-config-default)

;; Ido-Mode Initialization
(ido-mode 1)
(ido-everywhere 1)
(ido-vertical-mode 1)
(set 'ido-enable-flex-matching t)
(set 'ido-auto-merge-work-directories-length -1)

;; Flycheck-Mode Initialization
(global-flycheck-mode t)

;; Show the name of the current function in the modeline.
(which-function-mode)

;; Surround
(global-surround-mode 1)

(provide 'init-interface)
