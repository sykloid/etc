;;; autoload-provide-feature.el -- Emacs 24.4 workaround for *-autoloads features.
;;; P.C. Shyamshankar "sykora" <sykora@luceentbeing.com>

(require 'autoload)
(require 'package)

;; The built-in `autoload-rubric' function has a placeholder for the name of the feature to be
;; provided, but the built-in `autoload-ensure-default-file' doesn't use it.
(defun autoload-ensure-default-file (file)
  "Make sure that the autoload file FILE exists and if not create it."
  (message "%s" (file-name-base file))
  (unless (file-exists-p file)
    (write-region (autoload-rubric file nil (file-name-base file)) nil file))
  file)

;; As if that wasn't enough, `package.el' has its own version which ignores `autoload.el'.
(defalias 'package-autoload-ensure-default-file 'autoload-ensure-default-file)

(provide 'autoload-provide-feature)
