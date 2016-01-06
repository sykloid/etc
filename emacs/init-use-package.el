;; Emacs Initialization
;; P.C.Shyamshankar 'sykora' <sykora@lucentbeing.com>

;; This is rewrite #4.

;; Interpreter Configuration
(setq gc-cons-threshold 100000000)

;; Paths
(defvar user-lisp-directory (concat user-emacs-directory "lisp/"))
(add-to-list 'load-path user-lisp-directory)

(setq custom-theme-directory (concat user-lisp-directory "themes/"))

(setq custom-file (concat user-emacs-directory "customizations.el"))
(load custom-file 'no-error)

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

;; Appearance
(set-frame-font "Pragmata Pro-10")
(setc default-frame-alist '((font . "Pragmata Pro-10")))

(setc inhibit-splash-screen t)

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (load-theme 'skywave-gui))))
  (load-theme 'skywave-gui))

;; Miscellaneous
(setc ad-redefinition-action 'accept)

;; Package Initialization
(use-package winner
  :init (winner-mode t))

(use-package undo-tree
  :diminish undo-tree-mode)

(use-package hydra
  :ensure t
  :config
  (hydra-add-font-lock))

(use-package evil
  :ensure t
  :bind (:map evil-normal-state-map
              ("n" . evil-backward-char)
              ("e" . evil-next-visual-line)
              ("i" . evil-previous-visual-line)
              ("o" . evil-forward-char)

              ("N" . beginning-of-line-toggle)
              ("O" . end-of-line)

              ("h" . evil-insert-state)
              ("H" . evil-insert-line)
              ("a" . evil-append)
              ("A" . evil-append-line)
              ("y" . evil-open-below)
              ("Y" . evil-open-above)

              ("t" . evil-search-next)
              ("T" . evil-search-previous)

              ("k" . evil-yank)
              ("m" . evil-paste-after)
              ("M" . evil-paste-before)

              ("q" . nil)
              ("Q" . evil-record-macro)

              ("s" . evil-utility-map))

  :bind (:map evil-motion-state-map
              ("n" . evil-backward-char)
              ("e" . evil-next-visual-line)
              ("i" . evil-previous-visual-line)
              ("o" . evil-forward-char)

              ("t" . evil-search-next)
              ("T" . evil-search-previous))

  :bind (:map evil-visual-state-map
              ("n" . evil-backward-char)
              ("e" . evil-next-visual-line)
              ("i" . evil-previous-visual-line)
              ("o" . evil-forward-char)

              ("k" . evil-yank)
              ("m" . evil-paste-after)
              ("M" . evil-paste-before)

              ("y" . evil-visual-exchange-corners)

              ("h" . evil-inner-text-objects-map))

  :bind (:map evil-window-map
              ("n" . evil-window-left)
              ("e" . evil-window-down)
              ("i" . evil-window-up)
              ("o" . evil-window-right)

              ("C-n" . evil-window-left)
              ("C-e" . evil-window-down)
              ("C-i" . evil-window-up)
              ("C-o" . evil-window-right)

              ("q" . evil-window-delete)
              ("d" . delete-other-windows)
              ("D" . delete-other-windows-vertically)

              ("u" . winner-undo)
              ("U" . winner-redo))

  :bind (:map evil-utility-map
              ("v" . evil-visual-restore)
              ("m" . evil-mark-last-yank))

  :init
  (evil-mode t)

  (defun evil-mark-last-yank ()
    (interactive)
    (evil-visual-make-selection (evil-get-marker ?[) (evil-get-marker ?])))

  (defun beginning-of-line-toggle ()
    (interactive)
    (let ((current (point)))
      (back-to-indentation)
      (when (= current (point))
        (beginning-of-line))))

  :config
  (setc evil-move-beyond-eol t)
  (setc evil-split-window-below t)
  (setc evil-vsplit-window-right t)
  (setc evil-want-fine-undo nil)

  (define-prefix-command 'evil-utility-map)

  (define-key evil-operator-state-map "h" evil-inner-text-objects-map))

(use-package evil-leader
  :ensure t
  :init
  (global-evil-leader-mode t)
  (evil-leader/set-leader "SPC"))

(use-package evil-surround
  :ensure t
  :bind (:map evil-normal-state-map
              ("js" . evil-surround-region)
              ("jS" . evil-Surround-region))

  :bind (:map evil-visual-state-map
              ("js" . evil-surround-region)
              ("jS" . evil-Surround-region))
  :init
  (global-evil-surround-mode))

(use-package magit
  :ensure t
  :commands (magit-status magit-blame)
  :init
  (defhydra hydra-magit (:idle 1.0)
    "
Git Control
-----------
{_b_} Blame
{_s_} Status
{_q_} Quit
"
    ("b" magit-blame :color blue)
    ("s" magit-status :color blue)
    ("q" nil))

  (evil-leader/set-key "g" 'hydra-magit/body)

  :config
  (bind-keys :map magit-mode-map
             ("n" . evil-backward-char)
             ("e" . evil-next-line)
             ("i" . evil-previous-line)
             ("o" . evil-forward-char)

             ("M-i" . magit-section-up)))
(use-package company
  :ensure t
  :bind (:map evil-insert-state-map
              ("TAB" . company-indent-or-complete-common))
  :init
  (global-company-mode))

