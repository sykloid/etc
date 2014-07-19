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

;;;; Appearance

(after ('emacs)
  (set 'inhibit-splash-screen t)

  (blink-cursor-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)

  (set-fringe-style '(8 . 0))

  (set-frame-font "Fantasque Sans Mono-11"))

;;;; Packages

(after ('emacs)
  (require 'package)
  (set 'package-user-dir (concat user-emacs-directory "elpa"))
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
  (package-initialize))

(provide 'init)
;;; init.el ends here
