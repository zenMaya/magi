
;;; -*- lexical-binding: t -*-
(defalias 'yes-or-no-p 'y-or-n-p)

(setq version-control t
      kept-new-versions 10
      kept-old-versions 0
      delete-old-versions t
      backup-by-copying t
      vc-make-backup-files t)

(setq backup-directory-alist
      `(("." . "~/.local/share/emacs/saves/")))

;;; Bindings

(use-package emacs
  :bind
  ("C-z" . ignore)
  ("C-x C-z" . ignore)
  ("C-x =" . balance-windows)
  ("C-x C-r" . recentf-open-files)
  :config
  (setq-default indent-tabs-mode nil)
  (setq-default mouse-drag-copy-region t
                mouse-drag-and-drop-region t))

(delete-selection-mode t)

;; (defun n-move-beginning-of-line (arg)
;;   (interactive "^p")
;;   (setq arg (or arg 1))
;;   (when (/= arg 1)
;;     (let ((line-move-visual nil))
;;       (forward-line (- arg 1)))
;;     (let ((orig-point (point)))
;;       (move-beginning-of-line 1)
;;       (back-to-indentation))))

;; (define-key global-map [remap move-beginning-of-line] 'n-move-beginning-of-line)

;;; General

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion))))))

(use-package vertico
  :init
  (vertico-mode))

(use-package vertico-directory
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word)))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :bind (("C-c h" . consult-history)
	 ("C-x b" . consult-buffer)
	 ("M-y" . consult-yank-pop)
	 ("C-s" . consult-line)
	 ;; M-g bindings (goto-map)
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
	 ("M-g g" . consult-goto-line)             ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ;; M-s bindings (search-map)
	 ("M-s d" . consult-find)
	 ("M-s D" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s m" . consult-multi-occur)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ;; Isearch integration
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	 ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi))            ;; needed by consult-line to detect isearch)
  )

(use-package which-key
  :init
  (which-key-mode))

(use-package ace-jump-mode
  :bind
  ("C-c SPC" . ace-jump-char-mode))

(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-encoding-shell "bash")
  (add-to-list 'tramp-connection-properties
	       (list (regexp-quote "/docker:maya@web:")
		     "remote-shell" "/bin/bash")))

;; (use-package embark
;;   :bind (("C-." . embark-act)
;; 	 ("M-." . embark-dwim)))

;;; Themes
;;(nano-modeline-mode)
(load-theme 'nano-light t)
(set-face-attribute 'default nil :height 175)
(pixel-scroll-precision-mode t)

;;; Languages

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package corfu
  :config
  (setq corfu-auto t
	corfu-auto-prefix 2
	corfu-auto-delay 0
	corfu-cycle t
	corfu-quit-at-boundary nil
	corfu-preselect-first nil
	completions-detailed t)
  :bind
  (:map corfu-map
	;;		   ("SPC" . corfu-insert-separator)
	("TAB" . corfu-next)
	([tab] . corfu-next)
	("S-TAB" . corfu-previous)
	([backtab] . corfu-previous))
  :init
  (global-corfu-mode))

(defun corfu-enable-in-minibuffer ()
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    (corfu-mode 1)))

(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
(show-paren-mode t)

(use-package eglot
  :commands (eglot eglot-ensure)
  :hook ((csharp-mode . eglot-ensure))
  :config
  (setq eglot-workspace-configuration
	'((svelte
	   (plugin
	    (svelte
	     (rename (enable . t))
	     (selectionRange (enable . t))
	     (codeActions (enable . t)))))))
  (progn
    (add-to-list 'eglot-server-programs
		 `(csharp-mode . ("omnisharp-wrapper" "-lsp")))))

(use-package eldoc)

(context-menu-mode t)

(use-package eldoc-box
  :hook (eldoc-mode . eldoc-box-hover-mode))

(use-package smartparens
  :hook (prog-mode . smartparens-strict-mode))

;;; Web

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-engines-alist
	'(("svelte" . "\\.svelte\\'")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;;; Lisp

(use-package paredit
  :hook ((scheme-mode . paredit-mode)
	 (emacs-lisp-mode . paredit-mode)
	 (lisp-mode . paredit-mode)))

(use-package geiser
  :config
  (setq geiser-repl-per-project-p t))

(use-package geiser-guile
  :config
  (add-to-list 'geiser-guile-load-path "~/.config/guix/current/share/guile/site/3.0/")
  (setq geiser-guile-init-file "~/.config/guile/guile-geiser.scm"))

;;; Prolog
(use-package flymake-swi-prolog
  :hook (prolog-mode . flymake-swi-prolog-setup-backend))

;;; Coq

(use-package proof-general)

(use-package company-coq
  :hook (coq-mode . company-coq-mode))

(use-package coq-literal)

;;; Org

(org-babel-do-load-languages 'org-babel-load-languages
                             '((coq . t)))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Notes/roam/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}")))
  (org-roam-db-autosync-mode))

(use-package org-roam-export)

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))
