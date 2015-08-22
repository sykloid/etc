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
  (if (not (region-active-p))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(provide 'comment-dwim-toggle)
;;; comment-dwim-toggle.el ends here
