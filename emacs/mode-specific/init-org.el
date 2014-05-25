;; Org-Mode Initialization

;; Org is largely a text mode, we want auto-filling.
(auto-fill-mode t)
(set 'org-tags-column -100)
(set 'org-adapt-indentation nil)

;; Org/Evil Keybindings.
(define-key evil-normal-state-map (kbd "TAB") 'org-cycle)

;; Attachment Associations
(add-to-list 'org-file-apps '("\\.pdf$" . "okular %s"))

(set 'org-attach-auto-tag "#")
(set 'org-attach-file-list-property "ATTACHMENTS")

(defun attachment-last (x y)
  (if (string= "#" x)
      nil
    (if (string= "#" y)
	t
      (string< x y))))

(set 'org-tags-sort-function 'attachment-last)

;; (defadvice org-attach-attach )

(provide 'init-org)
