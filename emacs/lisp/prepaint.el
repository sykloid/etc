;;; prepaint.el -- Highlight C-style preprocessor directives.

;; Copyright (C) 2003,2007,2014 Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: c, languages, faces
;; Version: 0.0.1
;; URL: https://github.com/Lindydancer/prepaint

;; Prepaint is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; Prepaint is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;{{{ Documentation

;; *Prepaint* is an Emacs package that highlight C-style preprocessor
;; directives. The main feature is support for macros that span
;; multiple lines.
;;
;; Prepaint is implemented as two minor modes: `prepaint-mode' and
;; `prepaint-global-mode'. The former can be applied to individual
;; buffers and the latter to all buffers.

;; Usage:
;;
;; Place the source file in a directory in the load path. Add the
;; following lines to an appropriate init file:
;;
;;    (require 'prepaint)
;;
;; Activate this package by Customize, or by placing the following line
;; into the appropriate init file:
;;
;;    (prepaint-global-mode 1)
;;
;; This package use Fone Lock mode, so `font-lock-mode' or
;; `global-font-lock-mode' must be enabled (which it is by default).

;; Example:
;;
;; Below is a screenshot of a sample C file, demonstrating the effect
;; of this package:
;;
;; ![See doc/demo.png for screenshot of Prepaint mode](doc/demo.png)

;;}}}

;;; Code:

;;{{{ Dependencies

(eval-when-compile
  (require 'cl))

;;}}}
;;{{{ Variables

(defgroup prepaint nil
  "Highlight preprocessor directives for C-like languages."
  :group 'faces)


(defface prepaint-face
  '((((class color) (background light)) (:background "Grey85")))
  "Face for prepaint."
  :group 'prepaint)

(defcustom prepaint-modes '(c-mode c++-mode objc-mode)
  "List of major modes where Prepaint Global mode should be enabled."
  :group 'prepaint
  :type '(repeat symbol))

;;}}}
;;{{{ The modes

;;;###autoload
(define-minor-mode prepaint-mode
  "Minor mode that highlight preprocessor directives."
  nil
  nil
  nil
  :group 'prepaint
  (if prepaint-mode
      (prepaint-font-lock-add-keywords)
    (prepaint-font-lock-remove-keywords))
  (when font-lock-mode
    (font-lock-fontify-buffer)))


;;;###autoload
(define-global-minor-mode prepaint-global-mode prepaint-mode
  ;; Bizarre interface, `define-global-minor-mode' can automatically
  ;; disable the minor mode, but must have this code to turn it on.
  (lambda ()
    (when (apply 'derived-mode-p prepaint-modes)
      (prepaint-mode 1)))
  :group 'prepaint)

;;}}}
;;{{{ Match functions

;; The idea here is to use the font-lock "achored" model. We match the
;; "#" sign and then do a submatch line-by-line for the preprocessor
;; statement.
;;
;; The main match function `prepaint-match-statement-line' finds the
;; end of the preprocessor statement. The line-by-line matching
;; function simple match each line until it reaches the limit.
;;
;; Note: Should we match the entire statement as one single match,
;; Emacs would extend the highlighted area to the right side of the
;; display. With the current solution, the highligt stop at the last
;; character on the line.

(defvar prepaint-match-debug nil
  "When non-nil, messages are beging echoed.")

(defun prepaint-match-pre ()
  ;; Set up the line-by-line search.
  (if prepaint-match-debug
      (message "prepaint-match-pre called. Point is %s" (point)))
  ;; ----------
  ;; Tell font-lock not to stop after one or a few lines.
  (setq font-lock-multiline t)
  ;; Move the point to include the "#" part.
  (beginning-of-line)
  ;; ----------
  ;; Find the end of the preprocessor statement.
  ;;
  ;; (Note: Do not return "point-max"; it works but it really slows
  ;; down font-lock.)
  (save-excursion
    (while (progn
             (end-of-line)
             (and
              (eq (char-before) ?\\)
              (not (eobp))))
      (forward-line))
    (point)))                           ; Return new search limit.


(defun prepaint-match-statement-line (limit)
  "Match function for highlighting preprocessor statements."
  (if prepaint-match-debug
      (message "prepaint-match-statement-line called at %s with limit %s"
               (point) limit))
  ;; Match one line at a time until we hit the limit.
  (if (>= (point) limit)
      nil
    (looking-at "^.*$")                 ; Always true.
    (forward-line)
    t))


(defvar prepaint-font-lock-keywords
  '(("^\\s *#"
     (prepaint-match-statement-line
      (prepaint-match-pre)
      nil
      (0 'prepaint-face append t)))))

(defun prepaint-font-lock-add-keywords (&optional mode)
  "Install keywords into major MODE, or into current buffer if nil."
  (font-lock-add-keywords mode prepaint-font-lock-keywords t))

(defun prepaint-font-lock-remove-keywords (&optional mode)
  "Remove keywords from major MODE, or from current buffer if nil."
  (font-lock-remove-keywords mode prepaint-font-lock-keywords))

;;}}}
;;{{{ Profile support

;; The following (non-evaluated) section can be used to
;; profile this package using `elp'.
;;
;; Invalid indentation on purpose!

(cond (nil
(setq elp-function-list
      '(prepaint-match-statement-line))))

;;}}}

;;{{{ The end

(provide 'prepaint)

;;}}}

;;; prepaint.el ends here.
