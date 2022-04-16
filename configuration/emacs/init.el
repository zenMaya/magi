
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

(global-set-key (kbd "C-z") 'ignore)
(global-set-key (kbd "C-x C-z") 'ignore)

;;; General

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

(use-package embark
  :bind (("C-." . embark-act)
	 ("M-." . embark-dwim)))

;;; Themes
;;(nano-modeline-mode)
(load-theme 'nano-dark t)

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
		   corfu-preselect-first nil)
	     :bind
	     (:map corfu-map
;;		   ("SPC" . corfu-insert-separator)
		   ("TAB" . corfu-next)
		   ([tab] . corfu-next)
		   ("S-TAB" . corfu-previous)
		   ([backtab] . corfu-previous))
	     :init
	     (corfu-global-mode))

(defun corfu-enable-in-minibuffer ()
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    (corfu-mode 1)))

(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

;;; Lisp

(use-package paredit
  :hook ((scheme-mode . paredit-mode)
	 (emacs-lisp-mode . paredit-mode)
	 (lisp-mode . paredit-mode)))
  

(use-package geiser-guile
	     :config
	     (add-to-list 'geiser-guile-load-path "~/.config/guix/current/share/guile/site/3.0"))
