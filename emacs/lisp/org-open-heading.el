(require 'evil)
(require 'org)

(defun org-open-heading-below-and-insert ()
  "Open a heading below the current heading, and enter insert mode."
  (interactive)
  (progn
    (org-insert-heading-after-current)
    (evil-insert-state)))

(defun org-open-heading-above-and-insert ()
  "Open a heading above the current heading, and enter insert mode."
  (interactive)
  (progn
    (beginning-of-line)
    (org-insert-heading)
    (evil-insert-state)))

(provide 'org-open-heading)
