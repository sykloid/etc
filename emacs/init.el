(defvar mode-agnostic-init-directory
  (concat user-emacs-directory "mode-agnostic/")
  "Location of mode-agnostic init-*.el files.")
(add-to-list 'load-path mode-agnostic-init-directory)

(defvar mode-specific-init-directory
  (concat user-emacs-directory "mode-specific/")
  "Location of mode-specific init-*.el files.")
(add-to-list 'load-path mode-specific-init-directory)

(require 'init-overrides)
(require 'init-packages)
(require 'init-interface)
(require 'init-detect)

(set 'custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(provide 'init)
