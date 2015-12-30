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

  :config
  (setc evil-move-beyond-eol t)
  (setc evil-want-fine-undo nil)

  (define-prefix-command 'evil-utility-map)

  (define-key evil-operator-state-map "h" evil-inner-text-objects-map))

(use-package evil-leader
  :ensure t
  :init
  (global-evil-leader-mode t)
  (evil-leader/set-leader "SPC"))

(use-package magit
  :ensure t

  :init
  (defhydra magit-hydra (:idle 1.0)
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

  (evil-leader/set-key "g" 'magit-hydra/body))
