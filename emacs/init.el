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
    (add-to-list 'package-archive-enable-alist '("melpa" . 'ido-vertical-mode)))


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

  (define-key evil-visual-state-map "h" evil-inner-text-objects-map)
  (define-key evil-operator-state-map "h" evil-inner-text-objects-map)

  ;; Search

  (define-key evil-normal-state-map "t" 'evil-search-next)
  (define-key evil-normal-state-map "T" 'evil-search-previous)

  ;; Miscellaneous -- the `s'-map is my `g'-map.

  (define-key evil-normal-state-map "s" nil))

(provide 'init)
;;; init.el ends here
