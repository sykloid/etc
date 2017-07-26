;;; comment-dwim-toggle.el - Custom Comment Functionality
;;; P.C. Shyamshankar "sykora" <sykora@lucentbeing.com>

;;; Commentary:

;;; This function provides a convenient method to context dependently comment/uncomment.

;;;###autoload
(defun comment-dwim-toggle (&optional arg)
  "Context dependently toggle comment/uncomment.
If a region is active, comment/uncomment it. Otherwise,
comment/uncomment the current line."
  (interactive "*P")
  (comment-normalize-vars)
  (cond
   ((not (region-active-p))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
   ((org-in-src-block-p t)
    (org-babel-do-in-edit-buffer (comment-dwim-toggle)))
   (t (comment-dwim arg))))


(provide 'comment-dwim-toggle)
;;; comment-dwim-toggle.el ends here
