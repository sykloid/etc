;;; alist-utils -- Functions for manipulating association lists.
;;; P.C. Shyamshankar "sykora" <sykora@lucentbeing.com>

;;; Commentary:

;;; The emacs-lisp standard library is surprisingly sparse on alist modification functions, even
;;; though they are the simplest associative data structure present. This module provides an
;;; assorted array of alist modification functions.

(defmacro upsert-alist-with-default (key value default merge alist)
  "Update or insert association of `KEY' in `ALIST', passing
either the old association, or `DEFAULT' to `MERGE'. Returns
whether or not `KEY' existed in `ALIST'."
  (declare (indent defun))
  `(let ((present (assoc ,key ,alist)))
    (if present (progn (setcdr present (,merge (cdr present) ,value)) t)
       (progn (push `(,,key . ,(,merge ,default ,value)) ,alist) nil))))

(defmacro upsert-alist (key value alist)
  "Insert or update association of `KEY' to `VALUE' in `ALIST'.
Returns whether or not `KEY' existed in `ALIST'."
  (declare (indent defun))
  `(upsert-alist-with-default ,key ,value nil (lambda (x y) y) ,alist))

(provide 'alist-utils)
;;; alist-utils.el ends here
