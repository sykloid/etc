;;; init -- Custom Emacs Configuration.
;;; P.C. Shyamshankar "sykora" <sykora@lucentbeing.com>

;;; Commentary:

;;; This configuration method is inspired by MilkyPostman's single-file setup at
;;; `http://milkbox.net/note/single-file-master-emacs-configuration/'. It makes
;;; use of the same `after' macro, tweaked slightly to accommodate multiple load
;;; targets.

;;; I also take inspiration from an extension of the above approach described at
;;; `http://nullprogram.com/blog/2013/06/02/'. This method permits dependencies
;;; on more than one package, but also goes ahead and installs all packages.
;;; While I still fundamentally want the ability to install all missing packages
;;; mentioned in the configuration, I'd rather not have that happen without my
;;; explicit authorization.

;;; Code:

;;;; Scaffolding

(defmacro eval-after-load-all (features form)
  "Arrange that if, and only if, all `FEATURES' are loaded, `FORM' is evaluated."
  (declare (indent defun))
  (if (null (cdr features))
      `(eval-after-load ,(car features) ,form)
    `(eval-after-load ,(car features) (eval-after-load-all ,(cdr features) ,form))))

(defmacro after (features &rest body)
  "Arrange that if, and only if, all `FEATURES' are loaded, `BODY' is evaluated.
Additionally, `BODY' is wrapped in a lambda so that it is properly byte-compiled."
  (declare (indent defun))
  `(eval-after-load-all ,features ((lambda () (quote (progn ,@body))))))

(defmacro after! (features &rest body)
  "Arrange that if, and only if, all `FEATURES' are loaded, `BODY' is evaluated.
Additionally, `BODY' is wrapped in a lambda so that it is properly byte-compiled."
  (declare (indent defun))
  `(progn
     (mapc 'require (list ,@features))
     (eval-after-load-all ,features ((lambda () (quote (progn ,@body)))))))

;;;; Packages and Libraries

(after ('emacs)
  (require 'package)

  (set 'package-user-dir (concat user-emacs-directory "elpa"))
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
  (package-initialize)

  (require 'package-filter)

  (after ('package-filter)
    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
    (add-to-list 'package-archive-enable-alist '("melpa" . 'evil-surround))
    (add-to-list 'package-archive-enable-alist '("melpa" . 'ido-vertical-mode))
    (add-to-list 'package-archive-enable-alist '("melpa" . 'yaml-mode)))


  (defvar user-lisp-directory (concat user-emacs-directory "lisp/"))
  (add-to-list 'load-path user-lisp-directory)

  (defvar user-theme-directory (concat user-lisp-directory "themes/"))
  (add-to-list 'custom-theme-load-path user-theme-directory)

  (set 'custom-file (concat user-lisp-directory "customizations.el"))
  (load-library custom-file))

;;;; Appearance

(after ('emacs)
  (set 'inhibit-splash-screen t)

  (blink-cursor-mode -1)
  (column-number-mode)
  (fringe-mode '(8 . 0))
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)

  (set 'scroll-step 1)

  (set-frame-font "Fantasque Sans Mono-11")

  (if (display-graphic-p)
      (load-theme 'skywave-gui)
    (load-theme 'skywave-tty)))

;;;; Mode Detection

(after ('emacs)
  (add-to-list 'auto-mode-alist '("\\.ya?ml" . yaml-mode)))

;;; Evil/Keymaps

(after ('evil-autoloads)
  (evil-mode t))

(after ('evil)

  ;; Cursor Movement

  (define-key evil-normal-state-map "n" 'evil-backward-char)
  (define-key evil-normal-state-map "e" 'evil-next-line)
  (define-key evil-normal-state-map "i" 'evil-previous-line)
  (define-key evil-normal-state-map "o" 'evil-forward-char)

  (define-key evil-motion-state-map "n" 'evil-backward-char)
  (define-key evil-motion-state-map "e" 'evil-next-line)
  (define-key evil-motion-state-map "i" 'evil-previous-line)
  (define-key evil-motion-state-map "o" 'evil-forward-char)

  (define-key evil-visual-state-map "n" 'evil-backward-char)
  (define-key evil-visual-state-map "e" 'evil-next-line)
  (define-key evil-visual-state-map "i" 'evil-previous-line)
  (define-key evil-visual-state-map "o" 'evil-forward-char)

  ;; Window Movement

  (define-key evil-window-map "n" 'evil-window-left)
  (define-key evil-window-map "e" 'evil-window-down)
  (define-key evil-window-map "i" 'evil-window-up)
  (define-key evil-window-map "o" 'evil-window-right)

  (define-key evil-window-map "\C-n" 'evil-window-left)
  (define-key evil-window-map "\C-e" 'evil-window-down)
  (define-key evil-window-map "\C-i" 'evil-window-up)
  (define-key evil-window-map "\C-o" 'evil-window-right)

  ;; Entering Insert Mode

  (define-key evil-normal-state-map "h" 'evil-insert-state)
  (define-key evil-normal-state-map "H" 'evil-insert-line)
  (define-key evil-normal-state-map "a" 'evil-append)
  (define-key evil-normal-state-map "A" 'evil-append-line)
  (define-key evil-normal-state-map "y" 'evil-open-below)
  (define-key evil-normal-state-map "Y" 'evil-open-above)

  ;; Copying and Pasting

  (define-key evil-normal-state-map "k" 'evil-yank)
  (define-key evil-normal-state-map "m" 'evil-paste-after)
  (define-key evil-normal-state-map "M" 'evil-paste-before)

  (define-key evil-visual-state-map "k" 'evil-yank)
  (define-key evil-visual-state-map "m" 'evil-paste-after)
  (define-key evil-visual-state-map "M" 'evil-paste-before)

  ;; Text-Objects and Motions

  (define-key evil-visual-state-map "y" 'evil-visual-exchange-corners)
  (define-key evil-visual-state-map "h" evil-inner-text-objects-map)
  (define-key evil-operator-state-map "h" evil-inner-text-objects-map)

  ;; Search

  (define-key evil-normal-state-map "t" 'evil-search-next)
  (define-key evil-normal-state-map "T" 'evil-search-previous)
  (define-key evil-motion-state-map "t" 'evil-search-next)
  (define-key evil-motion-state-map "T" 'evil-search-previous)

  ;; Miscellaneous -- the `s'-map is my `g'-map.

  (define-key evil-normal-state-map "s" nil)
  (define-key evil-normal-state-map "sv" 'evil-visual-restore)
  (define-key evil-normal-state-map "sff" 'ffap)
  (define-key evil-normal-state-map "sfo" 'ffap-other-window))

;;; Evil-Args

(after! ('evil-args)

  (add-to-list 'evil-args-openers "<")
  (add-to-list 'evil-args-closers ">")

  (add-to-list 'evil-args-delimiters ";")

  (after ('evil)
    (define-key evil-inner-text-objects-map "," 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "," 'evil-outer-arg)

    (define-key evil-normal-state-map "]," 'evil-forward-arg)
    (define-key evil-normal-state-map "[," 'evil-backward-arg)
    (define-key evil-motion-state-map "]," 'evil-forward-arg)
    (define-key evil-motion-state-map "[," 'evil-backward-arg)))

;;; Evil-Leader

(after ('evil-leader-autoloads)
  (global-evil-leader-mode t)
  (evil-leader/set-leader "<SPC>"))

;;; Evil-Surround

(after ('evil-surround-autoloads)
  (global-evil-surround-mode t)

  (after ('evil)
    (define-key evil-normal-state-map "js" 'evil-surround-region)
    (define-key evil-normal-state-map "jS" 'evil-surround-region)
    (define-key evil-visual-state-map "js" 'evil-surround-region)
    (define-key evil-visual-state-map "jS" 'evil-Surround-region)))

;;;; Extensions

(after! ('comment-dwim-toggle)
  (after ('evil-leader)
    (evil-leader/set-key "c" 'comment-dwim-toggle)))

;;; Expand-Region

(after ('expand-region-autoloads)
  (after ('evil)
    (define-key evil-visual-state-map "." 'er/expand-region)))

;;; Flycheck

(after ('flycheck-autoloads)
  (global-flycheck-mode t))

;;; Ido*

(after! ('ido)
  (ido-mode t)
  (ido-everywhere t)

  (set 'ido-max-prospects 6)

  (after ('evil-leader)
    (evil-leader/set-key "pf" 'ido-find-file)
    (evil-leader/set-key "pb" 'ido-switch-buffer))

  (after ('ido-ubiquitous-autoloads)
    (ido-ubiquitous-mode t))

  (after ('ido-vertical-mode-autoloads)
    (ido-vertical-mode t))

  (defun ido-setup-bindings ()
    (progn
    (define-key ido-completion-map "\C-e" 'ido-next-match)
    (define-key ido-completion-map "\C-i" 'ido-prev-match)))

  (add-hook 'ido-setup-hook 'ido-setup-bindings))

;;; Magit

(after ('magit-autoloads)
  (after ('evil-leader)
    (evil-leader/set-key "gs" 'magit-status)
    (evil-leader/set-key "gb" 'magit-blame-mode)))

(after ('undo-tree)
  (after ('evil)
    (define-key evil-normal-state-map "u" 'undo-tree-undo)
    (define-key evil-normal-state-map "U" 'undo-tree-redo))

  (after ('evil-leader)
    (evil-leader/set-key "u" 'undo-tree-visualize)))

;;; Multiple-Cursors

(after ('multiple-cursors-autoloads)
  (after ('evil-leader)
    (evil-leader/set-key "mt" 'mc/mark-all-like-this-dwim)))

;;;; Major Modes

;; Emacs-Lisp

(after ('emacs)
  (add-hook 'emacs-lisp-mode-hook
    (lambda () (font-lock-add-keywords nil '(("\\<after!?\\>" . font-lock-keyword-face)))))
  (after ('paredit-autoloads)
    (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode t)))))

(provide 'init)
;;; init.el ends here
