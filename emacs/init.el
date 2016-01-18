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
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; Bootstrap use-package itself, if absent.
(unless (require 'use-package nil 'silent)
  (package-refresh-contents)
  (package-install 'use-package)
  (require 'use-package))

(defmacro setc (variable value)
  `(customize-set-variable ',variable ,value))

(defmacro with-hook (hook &rest body)
  "When `HOOK' is called, execute `BODY'."
  (declare (indent 1))
  `(add-hook ',hook (lambda () (progn ,@body)) t))

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

(set-fill-column 100)

;; Behaviour
(setc scroll-step 1)

;; Miscellaneous
(setc ad-redefinition-action 'accept)
(defalias 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Package Initialization
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

              ("N" . beginning-of-line-toggle)
              ("O" . end-of-line)

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
              ("D" . delete-other-windows-vertically))

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

(use-package undo-tree
  :diminish undo-tree-mode
  :bind (:map evil-normal-state-map
	      ("u" . undo-tree-undo)
	      ("U" . undo-tree-redo))

  :init
  (evil-leader/set-key "u" 'undo-tree-visualize)

  (evil-set-initial-state 'undo-tree-visualizer-mode 'emacs)

  (bind-keys :map undo-tree-visualizer-mode-map
	     ("n" . undo-tree-visualize-switch-branch-left)
	     ("e" . undo-tree-visualize-redo)
	     ("i" . undo-tree-visualize-undo)
	     ("o" . undo-tree-visualize-switch-branch-right)))

(use-package winner
  :init (winner-mode t)
  :bind (:map evil-window-map
	      ("u" . winner-undo)
	      ("U" . winner-redo)))

(use-package hydra
  :ensure t
  :init
  (defhydra hydra-zoom (:color amaranth :hint nil :idle 1.0)
    "
Zoom: {_e_} Out | {_i_} In | {_r_} Reset | {_q_} Quit
"
    ("e" text-scale-decrease)
    ("i" text-scale-increase)
    ("r" (text-scale-increase 0))
    ("q" nil))

  (evil-leader/set-key "z" 'hydra-zoom/body)

  :config
  (hydra-add-font-lock))

(use-package magit
  :ensure t
  :commands (magit-status magit-blame)
  :init
  (defhydra hydra-magit (:color blue :hint nil :idle 1.0)
    "
 Git Control: {_b_} Blame | {_s_} Status | {_q_} Quit
"
    ("b" magit-blame)
    ("s" magit-status)
    ("q" nil))

  (evil-leader/set-key "g" 'hydra-magit/body)

  :config
  (bind-keys :map magit-mode-map
             ("n" . evil-backward-char)
             ("e" . evil-next-line)
             ("i" . evil-previous-line)
             ("o" . evil-forward-char)

	     ("V" . set-mark-command)

	     ("M-i" . magit-section-up)))

(use-package counsel
  :ensure t
  :diminish t
  :bind (("M-x" . counsel-M-x))
  :init
  (ivy-mode)
  (defhydra hydra-list (:color blue :idle 1.0 :hint nil)
    "
 Buffers and Files
─────────────────────────────────────────
 {_l_} List Buffers | {_f_} List Files
 {_k_} Kill Buffer  | {_d_} List Directories
 {_o_} Other Buffer | {_m_} List Menu
─────────────────────────────────────────
 {_q_} Quit
"
    ("b" bury-buffer)
    ("d" cd)
    ("f" counsel-find-file)
    ("k" kill-this-buffer)
    ("l" switch-to-buffer)
    ("m" imenu)
    ("o" evil-buffer)
    ("q" nil))

  (evil-leader/set-key "b" 'hydra-list/body)

  :config
  (setc ivy-wrap t)

  (bind-keys :map ivy-minibuffer-map
	     ("C-e" . ivy-next-line)
	     ("C-i" . ivy-previous-line)

	     ("<tab>" . ivy-partial-or-done)))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)

  :config
  (evil-set-initial-state 'flycheck-error-list-mode 'emacs)

  (setc flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package company
  :ensure t
  :bind (:map evil-insert-state-map
              ("TAB" . company-indent-or-complete-common))
  :init
  (global-company-mode))

(use-package imenu
  :config
  (setc imenu-space-replacement "-"))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  (smartparens-global-strict-mode)

  :config
  (defhydra hydra-smartparens (:color amaranth :idle 1.0 :hint nil)
    "
 SExp Navigation/Manipulation
───────────────────────────────────────────────────
 {_n_} Previous | {_r_} Raise  | {_sn_} Slurp From Left
 {_e_} Down     | {_k_} Kill   | {_so_} Slurp From Right
 {_i_} Up       |           ^^ | {_bn_} Barf To Left
 {_o_} Next     |           ^^ | {_bo_} Barf To Right
───────────────────────────────────────────────────
 {_u_} Undo     | {_U_} Redo
───────────────────────────────────────────────────
 {_q_} Quit     | {_a_} Append | {_h_} Insert
"
    ("n" sp-backward-sexp)
    ("i" sp-up-sexp)
    ("e" sp-down-sexp)
    ("o" sp-forward-sexp)

    ("r" sp-raise-sexp)
    ("k" sp-kill-hybrid-sexp)

    ("sn" sp-backward-slurp-sexp)
    ("so" sp-forward-slurp-sexp)

    ("bn" sp-backward-barf-sexp)
    ("bo" sp-forward-barf-sexp)

    ("u" undo-tree-undo)
    ("U" undo-tree-redo)

    ("a" evil-append :color blue)
    ("h" evil-insert :color blue)

    ("q" nil))

  (bind-keys :map evil-utility-map
             ("k" . hydra-smartparens/body))

  (sp-pair "'" "'" :unless '(sp-point-after-word-p))

  (sp-with-modes '(emacs-lisp-mode lisp-interaction-mode)
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" "`" :actions nil)
    (sp-local-pair "`" "'" :when '(sp-in-string-p))))

(use-package help-fns+ :ensure t)

(use-package projectile
  :ensure t
  :init
  (defhydra hydra-projectile (:color teal :hint nil :idle 1.0)
    "
 Projectile
─────────────────────────────────────
 {_s_} Switch to Project | {_a_} Search
 {_g_} Version Control   | {_b_} Buffers
 {_d_} Change Directory  | {_f_} Files
─────────────────────────────────────
 {_q_} Quit
"
    ("s" projectile-switch-project :color red)
    ("g" projectile-vc)
    ("d" (cd (projectile-project-root)))

    ("b" projectile-switch-to-buffer)
    ("f" projectile-find-file-dwim)
    ("a" counsel-git-grep)

    ("q" nil))

  (evil-leader/set-key "p" 'hydra-projectile/body)

  :config
  (setc projectile-completion-system 'ivy))

;; Modes
(use-package lisp-mode
  :mode ("\\.el'" . emacs-lisp-mode)
  :config
  (with-hook emacs-lisp-mode-hook
    (add-to-list 'imenu-generic-expression
		 '("Used Packages" "^(use-package \\([a-zA-Z0-9\-]*\\)" 1))))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs'"
  :config
  (with-hook haskell-mode-hook
    (haskell-decl-scan-mode)
    (haskell-doc-mode)
    (haskell-indentation-mode)))

(use-package ledger-mode
  :ensure t
  :mode "\\.ldg'"
  :config
  (setc ledger-clear-whole-transactions t)
  (setc ledger-use-iso-dates t)
  (setc ledger-post-account-alignment-column 2)
  (setc ledger-post-amount-alignment-column 80)
  (setc ledger-reconcile-default-commodity "USD"))

(use-package org
  :ensure org-plus-contrib
  :mode "\\.org'"
  :init
  (defhydra hydra-org (:color blue :hint nil :idle 1.0)
    "
Org: {_a_} Agenda | {_c_} Capture | {_j_} Jump to Clock | {_q_} Quit
"
    ("a" org-agenda)
    ("c" org-capture)
    ("j" org-clock-goto)
    ("q" nil))

  (evil-leader/set-key "o" 'hydra-org/body)

  :config
  ;; Paths
  (setc org-directory "~/org/")
  (setc org-agenda-files '("~/org/agenda.org"))

  ;; Agenda
  (setc org-agenda-span 14)
  (setc org-agenda-start-on-weekday nil)

  (with-hook org-agenda-mode-hook
    (hl-line-mode))

  ;; Appearance
  (setq org-ellipsis "…")

  ;; Source Code
  (setc org-src-fontify-natively t)
  (setc org-src-preserve-indentation t)

  ;; Tags
  (setc org-agenda-tags-column -100)
  (setc org-tags-column -100)

  ;; Capture Templates
  (setc org-capture-templates
	`(("t" "Task" entry (file (concat org-directory "agenda.org"))
	   "* TODO %?" :kill-buffer t :prepend t)))

  (with-hook org-mode-hook
    (auto-fill-mode)))

(use-package rust-mode
  :ensure t
  :mode "\\.rs'")

(use-package racer
  :ensure t
  :init
  (with-hook rust-mode-hook
    (racer-mode))

  :config
  (setq racer-cmd "/home/sykora/.cargo/bin/racer")
  (setq racer-rust-src-path "/home/sykora/src/scratch/rust/rust/src"))
