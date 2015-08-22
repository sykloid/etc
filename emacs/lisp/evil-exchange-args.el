(require 'evil-args)
(require 'evil-exchange)

(defun first-arg-p ()
  (let ((current-location (point)))
    (save-excursion
      (evil-backward-arg 1)
      (eq (point) current-location))))

(defun last-arg-p ()
  (save-excursion
    (evil-forward-arg 1)
    (let ((current-position (point)))
      (evil-forward-arg 1)
      (eq (point) current-position))))

;;;###autoload
(defun evil-exchange-forward-arg ()
  (interactive)
  (unless (last-arg-p)
    (execute-kbd-macro [?v ?h ?, ?y escape])
    (execute-kbd-macro "sXsxh,],sxh,],")))

;;;###autoload
(defun evil-exchange-backward-arg ()
  (interactive)
  (unless (first-arg-p)
    (execute-kbd-macro [?v ?h ?, ?y escape])
    (execute-kbd-macro "sXsxh,[,sxh,[,")))

(provide 'evil-exchange-args)
