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

;;; Emacs

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt)
        ;; turn off bell
        visible-bell t)
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; don't indent with tabs
  (setq-default indent-tabs-mode nil)
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)
  (setq visible-bell t)
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  (global-prettify-symbols-mode t)
  (set-frame-font "Iosevka-14:weight=light" t t)
  :bind
  ("C-z" . ignore)
  ("C-x C-z" . ignore)
  ("C-x =" . balance-windows)
  ("C-x C-r" . recentf-open-files)
  ;; ("M-<up>" . windmove-up)
  ;; ("M-<down>" . windmove-down)
  ;; ("M-<right>" . windmove-right)
  ;; ("M-<left>" . windmove-left)
  :config
  (setq-default indent-tabs-mode nil)
  (setq-default mouse-drag-copy-region t
                mouse-drag-and-drop-region t)
  :hook
  (window-setup . toggle-frame-maximized)
  (text-mode . auto-fill-mode))

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL -c 'echo $PATH'"
						    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)


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

(setq user-full-name "Maya Tomasek")
(setq user-mail-address "maya.tomasek@disroot.org")

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;; (use-package disable-mouse
;;   :config
;;   (global-disable-mouse-mode))

;; workspaces
(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c C-p"))
  :init
  (persp-mode))

(use-package orderless
  :init
  (setq orderless-component-separator #'orderless-escapable-split-on-space
        completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package savehist
  :init
  (savehist-mode))

(use-package vertico
  :init
  (vertico-mode)
  :config
  (defadvice vertico-insert
      (after vertico-insert-add-history activate)
    "Make vertico-insert add to the minibuffer history."
    (unless (eq minibuffer-history-variable t)
      (add-to-history minibuffer-history-variable (minibuffer-contents)))))
  

(use-package vertico-directory
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package vertico-mouse
  :hook (vertico-mode . vertico-mouse-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(defvar consult--bufler-workspace+
  `(:name "Workspace"
          :narrow ?w
          :category buffer
          :face consult-buffer
          :history  buffer-name-history
          :state    ,#'consult--buffer-state
          :enabled  ,(lambda () (frame-parameter nil 'bufler-workspace-path))
          :items
          ,(lambda ()
             (let ((bufler-vc-state nil))
               (mapcar #'buffer-name
                       (mapcar #'cdr
                               (bufler-buffer-alist-at
                                (frame-parameter nil 'bufler-workspace-path)
                                :filter-fns bufler-filter-buffer-fns))))))
  "Bufler workspace buffers source for `consult-buffer'.")

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
  :config
  ;; (consult-customize consult--source-buffer :hidden t :default nil)
  ;; (add-to-list 'consult-buffer-sources persp-consult-source)
  (push #'consult--bufler-workspace+ consult-buffer-sources))

(use-package embark
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :bind*
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package bufler
  :config
  (bufler-mode))

(use-package which-key
  :init
  (which-key-mode))

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key))

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

(use-package sr-speedbar)

;;; Apps

(setq auth-sources '(password-store))
(setq auth-source-pass-filename "~/.password-store")

(setq sendmail-program "msmtp"
      message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from"))

(setq message-citation-line-format "On %Y-%m-%d %R, %N wrote:\n"
      message-auto-save-directory "~/.cache/emacs/mail/drafts")

(use-package notmuch
  :config
  (setq notmuch-address-selection-function (lambda (prompt collection initial-input)
                                             (completing-read prompt
                                                              (cons initial-input collection)
                                                              nil
                                                              t
                                                              nil
                                                              'notmuch-address-history))))

;; (use-package calibredb
;;   :config
;;   (setq calibredb-root-dir "~/Library/"
;;         calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)
;;         calibredb-library-alist '(("~/Library"))))

;; (use-package gnus
;;   :config
;;   (add-to-list 'gnus-secondary-select-methods '(nnimap "maya.tomasek@disroot.org"
;;                  (nnimap-address "disroot.org")
;;                  (nnimap-server-port 993)
;;                  (nnimap-stream ssl)))
;;   (add-to-list 'gnus-secondary-select-methods '(nntp "news.gwene.org")))

;; (setq send-mail-function 'smtpmail-send-it
;;         message-send-mail-function 'smtpmail-send-it
;;         smtpmail-default-smtp-server "disroot.org"
;;         smtpmail-smtpd-service 587
;;         smtpmail-stream-type 'starttls
;;         smtpmail-local-domain "disroot.org"
;;         smtpmail-sendto-domain "disroot.org")

;;(pinentry-start)

;;; Themes

(defun set-font-face-attributes ()
  (ef-themes-with-colors
    (set-face-attribute 'default nil :weight 'light)
    (set-face-attribute 'fixed-pitch nil :weight 'light)
    (set-face-attribute 'variable-pitch nil :weight 'light)
    (set-face-attribute 'custom-variable-button nil :weight 'regular)
    (set-face-attribute 'bold nil :weight 'regular)))

(use-package ef-themes
  :after org
  :init
  (add-hook 'ef-themes-post-load-hook #'set-font-face-attributes)
  (setq ;; ef-themes-mixed-fonts t ; this is broken, doesn't work with face-attributes of fixed pitch
        ef-themes-to-toggle '(ef-summer ef-winter)
        ef-themes-variable-pitch-ui t)
        ;; ef-themes-headings ; read the manual's entry or the doc string
        ;; '((0 . (variable-pitch light 1.9))
        ;;   (1 . (variable-pitch light 1.8))
        ;;   (2 . (variable-pitch regular 1.7))
        ;;   (3 . (variable-pitch regular 1.6))
        ;;   (4 . (variable-pitch regular 1.5))
        ;;   (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
        ;;   (6 . (variable-pitch 1.3))
        ;;   (7 . (variable-pitch 1.2))
        ;;   (t . (variable-pitch 1.1)))
        
  (mapc #'disable-theme custom-enabled-themes)
  :config
  (ef-themes-select 'ef-summer))
;;(nano-modeline-mode)
;;(load-theme 'nano-light t)
;;(set-face-attribute 'default nil :height 175)
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
        ;;                 ("SPC" . corfu-insert-separator)
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
  (setq eglot-extend-to-xref t
        eglot-workspace-configuration '((svelte
                                         (plugin
                                          (svelte
                                           (rename (enable . t))
                                           (selectionRange (enable . t))
                                           (codeActions (enable . t)))))))
  (progn
    (add-to-list 'eglot-server-programs
                 `(svelte-mode . ("svelteserver" "--stdio")))
    (add-to-list 'eglot-server-programs
                 `(csharp-mode . ("omnisharp-wrapper" "-lsp")))))

(use-package eldoc)

(context-menu-mode t)

(use-package eldoc-box
  :hook (eldoc-mode . eldoc-box-hover-mode))

(use-package parinfer-rust-mode
  :hook ((lisp-mode . parinfer-rust-mode)
         (scheme-mode . parinfer-rust-mode)))

;; (use-package smartparens-config
;;   :config (progn (show-smartparens-global-mode t))
;;   :hook ((prog-mode . turn-on-smartparens-strict-mode)
;;          (markdown-mode turn-on-smartparens-strict-mode))
;;   :bind (:map smartparens-mode-map
;;               ("C-M-a" . sp-beginning-of-sexp)
;;               ("C-M-e" . sp-end-of-sexp)
;;               ("C-M-SPC" . sp-mark-sexp)

;;               ("C-<down>" . sp-down-sexp)
;;               ("C-<up>"   . sp-up-sexp)
;;               ("M-<down>" . sp-backward-down-sexp)
;;               ("M-<up>"   . sp-backward-up-sexp)

;;               ("C-M-f" . sp-forward-sexp)
;;               ("C-M-b" . sp-backward-sexp)

;;               ("C-M-n" . sp-next-sexp)
;;               ("C-M-p" . sp-previous-sexp)

;;               ("C-S-f" . sp-forward-symbol)
;;               ("C-S-b" . sp-backward-symbol)

;;               ("C-<right>" . sp-forward-slurp-sexp)
;;               ("M-<right>" . sp-forward-barf-sexp)
;;               ("C-<left>"  . sp-backward-slurp-sexp)
;;               ("M-<left>"  . sp-backward-barf-sexp)

;;               ("C-M-t" . sp-transpose-sexp)
;;               ("C-M-k" . sp-kill-sexp)
;;               ("C-k"   . sp-kill-hybrid-sexp)
;;               ("M-k"   . sp-backward-kill-sexp)
;;               ("C-M-w" . sp-copy-sexp)
;;               ("C-M-d" . delete-sexp)

;;               ("M-<backspace>" . backward-kill-word)
;;               ("C-<backspace>" . sp-backward-kill-word)
;;               ([remap sp-backward-kill-word] . backward-kill-word)

;;               ("M-[" . sp-backward-unwrap-sexp)
;;               ("M-]" . sp-unwrap-sexp)

;;               ("C-x C-t" . sp-transpose-hybrid-sexp)

;;               ("C-c ("  . wrap-with-parens)
;;               ("C-c ["  . wrap-with-brackets)
;;               ("C-c {"  . wrap-with-braces)
;;               ("C-c '"  . wrap-with-single-quotes)
;;               ("C-c \"" . wrap-with-double-quotes)
;;               ("C-c _"  . wrap-with-underscores)
;;               ("C-c `"  . wrap-with-back-quotes)))

(use-package ansi-color
 :init
 (defun colorize-compilation-buffer ()
   (read-only-mode -1)
   (ansi-color-apply-on-region compilation-filter-start (point))
   (read-only-mode t))
 :hook (compilation-filter . colorize-compilation-buffer))

(use-package hideshowvis
  :config
  (setq hideshowvis-symbols t)
  :hook
  (prog-mode . hs-minor-mode)
  (prog-mode . hideshowvis-minor-mode))

;;; Web

;; (use-package web-mode
;;   :config
;; ;;  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
;; ;;  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;;   (setq web-mode-engines-alist
;;      '(("svelte" . "\\.svelte\\'")))
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-css-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2))

;;; Lisp

;; (use-package paredit
;;   :hook ((scheme-mode . paredit-mode)
;;       (emacs-lisp-mode . paredit-mode)
;;       (lisp-mode . paredit-mode)))

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

;;; Apl

(use-package gnu-apl-mode
  :config
  (setq gnu-apl-keymap-template "
╔════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦═════════╗
║ ±∇ ║ !∇ ║ @∇ ║ #∇ ║ $∇ ║ %∇ ║ ^∇ ║ &∇ ║ *∇ ║ (∇ ║ )∇ ║ _∇ ║ +∇ ║         ║
║ §∇ ║ 1∇ ║ 2∇ ║ 3∇ ║ 4∇ ║ 5∇ ║ 6∇ ║ 7∇ ║ 8∇ ║ 9∇ ║ 0∇ ║ -∇ ║ =∇ ║ BACKSP  ║
╠════╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦══════╣
║       ║ \"∇ ║ <∇ ║ >∇ ║ P∇ ║ Y∇ ║ F∇ ║ G∇ ║ C∇ ║ R∇ ║ L∇ ║ ?∇ ║ +∇ ║ RET  ║
║  TAB  ║ '∇ ║ ,∇ ║ .∇ ║ p∇ ║ y∇ ║ f∇ ║ g∇ ║ c∇ ║ r∇ ║ l∇ ║ /∇ ║ =∇ ║      ║
╠═══════╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╗    ║
║ (CAPS   ║ A∇ ║ O∇ ║ E∇ ║ U∇ ║ I∇ ║ D∇ ║ H∇ ║ T∇ ║ N∇ ║ S∇ ║ _∇ ║ |∇ ║    ║
║  LOCK)  ║ a∇ ║ o∇ ║ e∇ ║ u∇ ║ i∇ ║ d∇ ║ h∇ ║ t∇ ║ n∇ ║ s∇ ║ -∇ ║ \\∇ ║    ║
╠════════╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩════╩════╣
║        ║ ~∇ ║ Z∇ ║ X∇ ║ C∇ ║ V∇ ║ B∇ ║ N∇ ║ M∇ ║ <∇ ║ >∇ ║ ?∇ ║          ║
║  SHIFT ║ `∇ ║ z∇ ║ x∇ ║ c∇ ║ v∇ ║ b∇ ║ n∇ ║ m∇ ║ ,∇ ║ .∇ ║ /∇ ║  SHIFT   ║
╚════════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩══════════╝"))

;;; C/C++
(use-package semantic
  :config
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1))

;;; Org
(use-package org
  :config
  (setq org-latex-listings 'minted))

(use-package ox-latex
  :config
  (add-to-list 'org-latex-minted-langs '(kelp "scheme"))
  (let ((texcmd))
    (setq texcmd "latexmk -pdflatex='xelatex -file-line-error --shell-escape -synctex=1' -pdf %f")
    (setq org-latex-default-packages-alist
          '(("" "fontspec" t)
            ("" "xunicode" t)
            ("" "url" t)
            ("" "minted" t)
            ;; ("" "rotating" t)
            ;; ("" "memoir-article-styles" t)
            ;; ("american" "babel" t)
            ;; ("babel" "csquotes" t)
            ;; ("" "listings" nil)
            ("svgnames" "xcolor" t)
            ("" "soul" t)
            ("xetex, colorlinks=true, urlcolor=FireBrick, plainpages=false, pdfpagelabels, bookmarksnumbered" "hyperref" nil)))
    (setq org-latex-classes
          (cons '("memarticle"
                  "\\documentclass[11pt,oneside,article]{memoir}"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                org-latex-classes))
    (setq org-latex-pdf-process (list texcmd))))

(org-babel-do-load-languages 'org-babel-load-languages
                             '((coq . t)))

(use-package org-modern
  :hook
  ((org-mode . org-modern-mode)
   (org-agenda-finalize . org-modern-agenda)))

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
