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

(defmacro after! (features &rest body)
  "Load `FEATURES' now, and arrange that `BODY' is evaluated when they are.
Additionally, `BODY' is wrapped in a lambda so that it is properly byte-compiled."
  (declare (indent defun))
  `(progn
     (mapc 'require (list ,@features))
     (eval-after-load-all ,features ((lambda () (quote (progn ,@body)))))))

(defmacro with-hook (hook &rest body)
  "Add to the value of `HOOK', all of the actions in `BODY'."
  (declare (indent 1))
  `(add-hook ,hook (lambda () (progn ,@body))))

;;;; Packages and Libraries

(after ('emacs)
  (require 'package)

  (set 'package-user-dir (concat user-emacs-directory "elpa/"))
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
  (package-initialize)

  (after! ('package-filter)
    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
    (add-to-list 'package-archive-enable-alist '("melpa" . 'evil-surround))
    (add-to-list 'package-archive-enable-alist '("melpa" . 'ido-vertical-mode))
    (add-to-list 'package-archive-enable-alist '("melpa" . 'yaml-mode)))


  (defvar user-lisp-directory (concat user-emacs-directory "lisp/"))
  (add-to-list 'load-path user-lisp-directory)

  (defvar user-theme-directory (concat user-lisp-directory "themes/"))
  (add-to-list 'custom-theme-load-path user-theme-directory)

  (set 'custom-file (concat user-lisp-directory "customizations.el"))
  (load-library custom-file))

;;;; Mode Detection

(after ('emacs)

  (autoload 'K3-mode "K3-mode")
  (add-to-list 'auto-mode-alist '("\\.k3" . K3-mode))

  (add-to-list 'auto-mode-alist '("\\.ldg" . ledger-mode))

  (add-to-list 'auto-mode-alist '("\\.ya?ml" . yaml-mode)))

;;;; Appearance

(after ('emacs)
  (set 'inhibit-splash-screen t)

  (blink-cursor-mode -1)
  (column-number-mode)
  (fringe-mode '(8 . 0))
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)

  (set 'scroll-step 1)

  (set-frame-font "Fantasque Sans Mono-11")

  (if (display-graphic-p)
      (load-theme 'skywave-gui)
    (load-theme 'skywave-tty))

  (defalias 'yes-or-no-p 'y-or-n-p))

;;; Interface

(after ('emacs)
  (set-default 'fill-column 100)
  (set-default 'show-trailing-whitespace t)

  (set 'sentence-end-double-space nil))

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

  (define-key evil-visual-state-map "y" 'evil-visual-exchange-corners)
  (define-key evil-visual-state-map "h" evil-inner-text-objects-map)
  (define-key evil-operator-state-map "h" evil-inner-text-objects-map)

  ;; Search

  (define-key evil-normal-state-map "t" 'evil-search-next)
  (define-key evil-normal-state-map "T" 'evil-search-previous)
  (define-key evil-motion-state-map "t" 'evil-search-next)
  (define-key evil-motion-state-map "T" 'evil-search-previous)

  ;; Miscellaneous -- the `s'-map is my `g'-map.

  (define-key evil-normal-state-map "s" nil)
  (define-key evil-normal-state-map "sv" 'evil-visual-restore)
  (define-key evil-normal-state-map "sff" 'ffap)
  (define-key evil-normal-state-map "sfo" 'ffap-other-window))

;;; Evil-Args

(after! ('evil-args)

  (add-to-list 'evil-args-openers "<")
  (add-to-list 'evil-args-closers ">")

  (add-to-list 'evil-args-delimiters ";")

  (after ('evil)
    (define-key evil-inner-text-objects-map "," 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "," 'evil-outer-arg)

    (define-key evil-normal-state-map "]," 'evil-forward-arg)
    (define-key evil-normal-state-map "[," 'evil-backward-arg)
    (define-key evil-motion-state-map "]," 'evil-forward-arg)
    (define-key evil-motion-state-map "[," 'evil-backward-arg)))

;;; Evil-Leader

(after ('evil-leader-autoloads)
  (global-evil-leader-mode t)
  (evil-leader/set-leader "<SPC>"))

;;; Evil-Surround

(after ('evil-surround-autoloads)
  (global-evil-surround-mode t)

  (after ('evil)
    (define-key evil-normal-state-map "js" 'evil-surround-region)
    (define-key evil-normal-state-map "jS" 'evil-surround-region)
    (define-key evil-visual-state-map "js" 'evil-surround-region)
    (define-key evil-visual-state-map "jS" 'evil-Surround-region)))

;;;; Extensions

(after! ('comment-dwim-toggle)
  (after ('evil-leader)
    (evil-leader/set-key "c" 'comment-dwim-toggle)))

;;; Compilation
(after ('compile)
  (set 'compilation-scroll-output t))

;;; Company
(after ('company-autoloads)
  (global-company-mode t))

(after ('company)
  (set 'company-dabbrev-downcase nil)
  (set 'company-dabbrev-ignore-case nil)
  (set 'company-dabbrev-code-ignore-case nil)

  (after ('evil)
    (define-key company-active-map "\C-e" 'company-select-next)
    (define-key company-active-map "\C-i" 'company-select-previous)))

(after ('etags)
  (after ('evil-leader)
    (evil-leader/set-key "tb" 'pop-tag-mark)
    (evil-leader/set-key "tr" 'find-tag)
    (evil-leader/set-key "tf" 'find-tag)))

;;; Expand-Region
(after ('expand-region-autoloads)
  (after ('evil)
    (define-key evil-visual-state-map "." 'er/expand-region)))

;;; ffap
(after! ('ffap))

;;; Flycheck
(after ('flycheck-autoloads)
  (global-flycheck-mode t)

  (after ('evil-leader)
    (evil-leader/set-key "fe" 'flycheck-next-error)
    (evil-leader/set-key "fi" 'flycheck-previous-error)
    (evil-leader/set-key "fl" 'flycheck-list-errors)
    (evil-leader/set-key "fc" 'flycheck-buffer)))

;;; Ido*

(after! ('ido)
  (ido-mode t)
  (ido-everywhere t)

  (set 'ido-auto-merge-work-directories-length -1)
  (set 'ido-cannot-complete-command 'ido-next-match)
  (set 'ido-max-prospects 6)

  (after ('evil-leader)
    (evil-leader/set-key "pb" 'ido-switch-buffer)
    (evil-leader/set-key "pf" 'ido-find-file)
    (evil-leader/set-key "pk" 'ido-kill-buffer)
    (evil-leader/set-key "pm" 'imenu))

  (after ('ido-ubiquitous-autoloads)
    (ido-ubiquitous-mode t))

  (after ('ido-vertical-mode-autoloads)
    (ido-vertical-mode t))

  (with-hook 'ido-setup-hook
    (define-key ido-completion-map "\C-e" 'ido-next-match)
    (define-key ido-completion-map "\C-i" 'ido-prev-match)
    (define-key ido-completion-map "\C-o" 'ido-restrict-to-matches)

    (define-key ido-completion-map (kbd "<tab>") 'ido-complete-space)
    (define-key ido-completion-map (kbd "<backtab>") 'ido-prev-match)))

;;; Magit

(after ('magit-autoloads)
  (after ('evil)
    (evil-define-key 'emacs magit-status-mode-map "n" 'evil-backward-char)
    (evil-define-key 'emacs magit-status-mode-map "e" 'evil-next-line)
    (evil-define-key 'emacs magit-status-mode-map "i" 'evil-previous-line)
    (evil-define-key 'emacs magit-status-mode-map "o" 'evil-forward-char)

    (evil-define-key 'emacs magit-status-mode-map "\S-n" 'magit-goto-previous-sibling-section)
    (evil-define-key 'emacs magit-status-mode-map "\S-e" 'magit-goto-next-section)
    (evil-define-key 'emacs magit-status-mode-map "\S-i" 'magit-goto-previous-section)
    (evil-define-key 'emacs magit-status-mode-map "\S-o" 'magit-goto-next-sibling-section))

  (after ('evil-leader)
    (evil-leader/set-key "gs" 'magit-status)
    (evil-leader/set-key "gb" 'magit-blame-mode)))

(after! ('smex)
  (smex-initialize)
  (global-set-key "\M-x" 'smex)
  (global-set-key "\M-X" 'smex-major-mode-commands)
  (global-set-key "\C-c\C-c\M-x" 'execute-extended-command))

(after ('undo-tree)
  (after ('evil)
    (define-key evil-normal-state-map "u" 'undo-tree-undo)
    (define-key evil-normal-state-map "U" 'undo-tree-redo))

  (after ('evil-leader)
    (evil-leader/set-key "u" 'undo-tree-visualize)))

;;; Multiple-Cursors

(after ('multiple-cursors-autoloads)
  (after ('evil-leader)
    (evil-leader/set-key "mt" 'mc/mark-all-like-this-dwim)))

;;;; Major Modes

;; TeX/LaTeX
(after ('auctex-autoloads)
  (set 'font-latex-fontify-sectioning 'color)
  (set 'TeX-newline-function 'reindent-then-newline-and-indent)

  (TeX-global-PDF-mode t))

(after ('cc-mode)
  (after! ('prepaint)
    (prepaint-global-mode t)
    (with-hook 'c-mode-common-hook
      (face-remap-add-relative 'prepaint-face 'font-lock-variable-name-face))))

;; Emacs-Lisp

(after ('emacs)
  (after ('paredit-autoloads)
    (with-hook 'emacs-lisp-mode-hook
      (paredit-mode t)))

  (with-hook 'emacs-lisp-mode-hook
    (turn-on-eldoc-mode)

    (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)

    (font-lock-add-keywords nil '(("(\\(\\<after!?\\>\\)" 1 'font-lock-keyword-face)))
    (font-lock-add-keywords nil '(("(\\(\\<with-hook\\>\\)" 1 'font-lock-keyword-face)))))

;; Haskell

(after ('haskell-mode)
  (with-hook 'haskell-mode-hook
    (turn-on-haskell-decl-scan)
    (turn-on-haskell-doc)
    (turn-on-haskell-indent)

    (face-remap-add-relative 'font-lock-doc-face 'font-lock-comment-face))

  (after ('flycheck)
    (with-hook 'flycheck-mode-hook
      #'flycheck-haskell-setup)))

;; Ledger

(after ('ledger-mode-autoloads)
  (set 'ledger-clear-whole-transactions t)
  (set 'ledger-post-amount-alignment-column 80)
  (set 'ledger-reconcile-default-commodity "USD")
  (set 'ledger-reports
       '(("account" "ledger -f %(ledger-file) register %(account)")
	 ("balance" "ledger -f %(ledger-file) balance")
	 ("payee" "ledger -f %(ledger-file) register @%(payee)")
	 ("register" "ledger -f %(ledger-file) register")))

  ; Not really specific to ledger, but close enough.
  (set 'pcomplete-termination-string "")

  (after ('evil)
    (evil-set-initial-state 'ledger-report-mode 'emacs))

  (after ('evil-leader)
    (evil-leader/set-key-for-mode 'ledger-mode "lq" 'ledger-post-align-xact)
    (evil-leader/set-key-for-mode 'ledger-mode "lr" 'ledger-report))

  (after ('flycheck)
    (require 'flycheck-ledger)
    (set 'flycheck-ledger-pedantic t)))

;; Org-Mode

(after ('org)
  (set 'org-adapt-indentation nil)

  (set 'org-edit-src-indentation 0)
  (set 'org-ellipsis " ↩")
  (set 'org-emphasis-alist
       '(("*" org-bold "<strong>" "</strong>")
  	 ("/" org-italic "<em>" "</em>")
  	 ("=" org-code "<code>" "</code>")
  	 ("~" org-verbatim "<code>" "</code>")))

  (set 'org-src-fontify-natively t)

  (after ('evil)
    (evil-define-key 'normal org-mode-map "\M-n" 'org-metaleft)
    (evil-define-key 'normal org-mode-map "\M-e" 'org-metadown)
    (evil-define-key 'normal org-mode-map "\M-i" 'org-metaup)
    (evil-define-key 'normal org-mode-map "\M-o" 'org-metaright)

    (evil-define-key 'normal org-mode-map "\S-n" 'org-shiftleft)
    (evil-define-key 'normal org-mode-map "\S-e" 'org-shiftdown)
    (evil-define-key 'normal org-mode-map "\S-i" 'org-shiftup)
    (evil-define-key 'normal org-mode-map "\S-o" 'org-shiftright)

    (evil-define-key 'insert org-mode-map "\M-n" 'org-metaleft)
    (evil-define-key 'insert org-mode-map "\M-e" 'org-metadown)
    (evil-define-key 'insert org-mode-map "\M-i" 'org-metaup)
    (evil-define-key 'insert org-mode-map "\M-o" 'org-metaright)

    (evil-define-key 'normal org-mode-map "\M-\S-n" 'org-shiftmetaleft)
    (evil-define-key 'normal org-mode-map "\M-\S-e" 'org-shiftmetadown)
    (evil-define-key 'normal org-mode-map "\M-\S-i" 'org-shiftmetaup)
    (evil-define-key 'normal org-mode-map "\M-\S-o" 'org-shiftmetaright)

    (after! ('org-open-heading)
      (evil-define-key 'normal org-mode-map "\M-y" 'org-open-heading-below-and-insert)
      (evil-define-key 'normal org-mode-map "\M-Y" 'org-open-heading-above-and-insert)))

  (with-hook 'org-mode-hook
    (auto-fill-mode t)))

(provide 'init)
;;; init.el ends here
