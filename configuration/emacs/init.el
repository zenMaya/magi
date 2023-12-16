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

  (defun stop-using-minibuffer ()
    "kill the minibuffer"
    (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
      (abort-recursive-edit)))


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
  (setq default-frame-alist '((font . "FiraCode-11:weight=light")
                              (vertical-scroll-bars)))
                                        ;(set-frame-font "FiraCode-11:weight=light" t t)
  (setq switch-to-buffer-obey-display-actions t
        window-sides-slots '(1 1 1 1))
  :bind
  ("C-d" . delete-forward-char) ;; somehow this is not the default
  ("C-z" . ignore)
  ("C-x C-p" . ignore)
  ("C-x C-z" . ignore)
  ("C-x =" . balance-windows)
  ("C-x C-r" . recentf-open-files)
  ("C-r" . rectangle-mark-mode)
  ("M-<up>" . windmove-up)
  ("M-<down>" . windmove-down)
  ("M-<right>" . windmove-right)
  ("M-<left>" . windmove-left)
  :config
  (setq compilation-scroll-output t)
  (setq-default indent-tabs-mode nil)
  (setq-default mouse-drag-copy-region 'non-empty
                mouse-drag-and-drop-region t
                mouse-drag-and-drop-region-cross-program t
                mouse-drag-and-drop-region-scroll-margin t)
  (setq set-message-function #'set-multi-message)
  (setq display-buffer-alist
        '(("\\ -\\ compilation\\*"
           (display-buffer-in-side-window)
           (side . bottom)
           (slot . 0))
          ("\\*vterminal\\ -\\ "
           (display-buffer-in-side-window)
           (side . bottom)
           (slot . 0))
          ("\\*Flymake\\ diagnostics\\ for\\ "
           (display-buffer-in-side-window)
           (side . bottom)
           (slot . 0))
          ("magit:\\ "
           (display-buffer-in-side-window)
           (side . left)
           (slot . 0))))
  :hook
  (mouse-leave-buffer . stop-using-minibuffer)
  (text-mode . auto-fill-mode)
  (after-save . delete-trailing-whitespace))

(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)))

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$" "" (shell-command-to-string
                                          "$SHELL -c 'echo $PATH'"))))

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

(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                          '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                            ;; =:= =!=
                            ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                            ;; ;; ;;;
                            (";" (rx (+ ";")))
                            ;; && &&&
                            ("&" (rx (+ "&")))
                            ;; !! !!! !. !: !!. != !== !~
                            ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                            ;; ?? ??? ?:  ?=  ?.
                            ("?" (rx (or ":" "=" "\." (+ "?"))))
                            ;; %% %%%
                            ("%" (rx (+ "%")))
                            ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                            ;; |->>-||-<<-| |- |== ||=||
                            ;; |==>>==<<==<=>==//==/=!==:===>
                            ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                            "-" "="))))
                            ;; \\ \\\ \/
                            ("\\" (rx (or "/" (+ "\\"))))
                            ;; ++ +++ ++++ +>
                            ("+" (rx (or ">" (+ "+"))))
                            ;; :: ::: :::: :> :< := :// ::=
                            (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                            ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                            ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                            "="))))
                            ;; .. ... .... .= .- .? ..= ..<
                            ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                            ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                            ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                            ;; *> */ *)  ** *** ****
                            ("*" (rx (or ">" "/" ")" (+ "*"))))
                            ;; www wwww
                            ("w" (rx (+ "w")))
                            ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                            ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                            ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                            ;; << <<< <<<<
                            ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                            "-"  "/" "|" "="))))
                            ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                            ;; >> >>> >>>>
                            (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                            ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                            ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                         (+ "#"))))
                            ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                            ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                            ;; __ ___ ____ _|_ __|____|_
                            ("_" (rx (+ (or "_" "|"))))
                            ;; Fira code: 0xFF 0x12
                            ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                            ;; Fira code:
                            "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                            ;; The few not covered by the regexps.
                            "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; (use-package disable-mouse
;;   :config
;;   (global-disable-mouse-mode))

;; workspaces
;; (use-package persp-mode
;;   :custom
;;   (persp-keymap-prefix (kbd "C-c C-p"))
;;   (persp-mode-prefix-key (kbd "C-c C-p"))
;;   ;; see documentation for other possible values
;;   (persp-add-buffer-on-after-change-major-mode t)
;;   :init
;;   (persp-mode))

(use-package orderless
  :init
  (setq orderless-component-separator #'orderless-escapable-split-on-space
        completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))
        orderless-matching-styles '(orderless-flex)))

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

;; (defvar consult--bufler-workspace+
;;   `(:name "Workspace"
;;           :narrow ?w
;;           :category buffer
;;           :face consult-buffer
;;           :history  buffer-name-history
;;           :state    ,#'consult--buffer-state
;;           :enabled  ,(lambda () (frame-parameter nil 'bufler-workspace-path))
;;           :items
;;           ,(lambda ()
;;              (let ((bufler-vc-state nil))
;;                (mapcar #'buffer-name
;;                        (mapcar #'cdr
;;                                (bufler-buffer-alist-at
;;                                 (frame-parameter nil 'bufler-workspace-path)
;;                                 :filter-fns bufler-filter-buffer-fns))))))
;;   "Bufler workspace buffers source for `consult-buffer'.")

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
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch)
         :map project-prefix-map
         ("b" . consult-project-buffer))
  :config
  ;; (consult-customize consult--source-buffer :hidden t :default nil)
  ;; (add-to-list 'consult-buffer-sources persp-consult-source)
  ;; (add-to-list 'consult-buffer-sources persp-buffer-list-function)
  ;; (push #'consult--buflers-workspace+ consult-buffer-sources)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; (defvar consult--source-persp-mode
;;   (list
;;    :name "Persp"
;;    :narrow ?s
;;    :state #'consult-buffer-state
;;    :default t
;;    :items (lambda ()
;;             (with-persp-buffer-list ()
;;                                     (consult--buffer-query :sort 'visibility
;;                                                            :as #'buffer-name)))))
;; (add-to-list 'consult-buffer-sources 'consult--source-persp-mode)

;; (use-package embark
;;   :init
;;   (setq prefix-help-command #'embark-prefix-help-command)
;;   :bind*
;;   (("C-." . embark-act)
;;    ("M-." . embark-dwim)
;;    ("C-h B" . embark-bindings))

;; :config
;; (add-to-list 'display-buffer-alist
;;              '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                nil
;;                (window-parameters (mode-line-format . none)))))

;; (use-package embark-consult
;;   :after (embark consult)
;;   :demand t
;;   :hook (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package bufler
;;   :config
;;   (bufler-mode))

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

;; (use-package popwin
;;   :config (popwin-mode))

(use-package multiple-cursors
  :config
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))

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
(use-package erc
  :config
  (setq erc-nick "zenmaya"
        erc-user-full-name "Maya"))

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
  (ef-themes-select 'ef-winter))
;;(nano-modeline-mode)
;;(load-theme 'nano-light t)
;;(set-face-attribute 'default nil :height 175)
(pixel-scroll-precision-mode t)

;;; Languages

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  ;;  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-file)
  :hook
  (org-mode . (add-to-list 'completion-at-point-functions #'cape-ispell)))

(use-package corfu
  :config
  (setq corfu-auto t
        corfu-auto-prefix 2
        corfu-auto-delay 0
        corfu-preview-current nil
        corfu-cycle t
        corfu-quit-no-match t
        corfu-preselect-first nil
        completions-detailed t)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
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

(use-package kind-icon
  :custom (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
;; (use-package company
;;   :bind
;;   (:map company-active-map
;;     ("TAB" . company-select-next)
;;     ([tab] . company-select-next))
;;   :config
;;   (setq company-minimum-prefix-length 1
;;         company-idle-delay 0
;;         company-selection-wrap-around 1
;;         company-tooltip-align-annotations 1
;;         company-format-margin-function #'company-vscode-dark-icons-margin)
;;   (global-company-mode))

(show-paren-mode t)

(use-package flymake
  :config
  (setq flymake-no-changes-timeout 0.5))

(defun project-root-override (dir)
  "Find DIR's project root by searching for a '.project.el' file.

If this file exists, it marks the project root. For convenient compatibility
with Projectile, '.projectile' is also considered a project root marker.

https://blog.jmthornton.net/p/emacs-project-override"
  (let ((root (or (locate-dominating-file dir ".project.el")
                  (locate-dominating-file dir ".projectile")))
        (backend (ignore-errors (vc-responsible-backend dir))))
    (when root (if (version<= emacs-version "28")
                   (cons 'vc root)
                 (list 'vc backend root)))))

(use-package project
  :config
  (add-hook 'project-find-functions #'project-root-override))

(use-package eglot
  :commands (eglot eglot-ensure)
  :hook
  (eglot-connect . (lambda (&rest _) (flymake-show-project-diagnostics)))
  :bind (("C-c e r" . eglot-rename)
         ("C-c e d" . eglot-find-typeDefinition)
         ("C-c e D" . eglot-find-declaration)
         ("C-c e f" . eglot-format)
         ("C-c e F" . eglot-format-buffer)
         ("C-c e R" . eglot-reconnect))
  :config
  (setq eglot-extend-to-xref t
        eglot-workspace-configuration '((svelte
                                         (plugin
                                          (svelte
                                           (rename (enable . t))
                                           (selectionRange (enable . t))
                                           (codeActions (enable . t))))))
        eglot-events-buffer-size 0
        completion-category-defaults nil
        eglot-ignored-server-capabilities '(:hoverProvider :documentHighlightProvider :inlayHint))
  (progn
    (add-to-list 'eglot-server-programs
                 `(svelte-mode . ("svelteserver" "--stdio")))
    (add-to-list 'eglot-server-programs
                 `(csharp-mode . ("omnisharp-wrapper" "-lsp")))
    (add-to-list 'eglot-server-programs
                 `(fsharp-mode . ("fsautocomplete" "--adaptive-lsp-server-enabled" "-v"
                                  :initializationOptions '(
                                                           :automaticWorkspaceInit t
                                                           :abstractClassStubGeneration t
				                           :abstractClassStubGenerationMethodBody
				                           "failwith \"Not Implemented\""
				                           :abstractClassStubGenerationObjectIdentifier "this"
				                           :addFsiWatcher nil
				                           :codeLenses (:references (:enabled t)
							                            :signature (:enabled t))
				                           :disableFailedProjectNotifications nil
				                           :dotnetRoot ""
				                           :enableAdaptiveLspServer t
				                           :enableAnalyzers nil
				                           :enableMSBuildProjectGraph nil
				                           :enableReferenceCodeLens t
				                           :excludeProjectDirectories [".git" "paket-files" ".fable" "packages" "node_modules"]
				                           :externalAutocomplete nil
				                           :fsac (:attachDebugger nil
                                                                                  :cachedTypeCheckCount 200
				                                                  :conserveMemory nil
				                                                  :dotnetArgs nil
				                                                  :netCoreDllPath ""
				                                                  :parallelReferenceResolution nil
				                                                  :silencedLogs nil)
				                           :fsiExtraParameters nil
				                           :fsiSdkFilePath ""
				                           :generateBinlog nil
				                           :indentationSize 4
				                           :inlayHints (:disableLongTooltip nil
								                            :enabled t
								                            :parameterNames t
								                            :typeAnnotations t)
				                           :inlineValues (:enabled nil
							                           :prefix "//")
				                           :interfaceStubGeneration t
				                           :interfaceStubGenerationMethodBody "failwith \"Not Implemented\""
				                           :interfaceStubGenerationObjectIdentifier "this"
				                           :keywordsAutocomplete t
				                           :lineLens (:enabled "replaceCodeLens"
					                                       :prefix " // ")
				                           :linter t
				                           :pipelineHints (:enabled t
							                            :prefix " // ")
				                           :recordStubGeneration t
				                           :recordStubGenerationBody "failwith \"Not Implemented\""
				                           :resolveNamespaces t
				                           :saveOnSendLastSelection nil
				                           :simplifyNameAnalyzer t
				                           :smartIndent nil
				                           :suggestGitignore t
				                           :suggestSdkScripts t
				                           :unionCaseStubGeneration t
				                           :unionCaseStubGenerationBody "failwith \"Not Implemented\""
				                           :unusedDeclarationsAnalyzer t
				                           :unusedOpensAnalyzer t
				                           :verboseLogging nil
				                           :workspaceModePeekDeepLevel 4
				                           :workspacePath ""))))
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)))

  ;; (use-package lsp-mode
  ;;   :hook
  ;;   ;;(zig-mode . lsp)
  ;;   (lsp-mode . lsp-enable-which-key-integration)
  ;;   (lsp-mode . yas-minor-mode)
  ;;   :commands
  ;;   lsp
  ;;   :config
  ;;   (setq lsp-eldoc-enable-hover nil))
  ;; (use-package lsp-ui
  ;;   :config
  ;;   (setq lsp-ui-doc-enable 't
  ;;         lsp-ui-doc-show-with-cursor 't))

  ;; ctags
  ;; (defun create-tags (&optional dir-name)
  ;;   "Create TAGS file."
  ;;   (interactive "DSource Directory: ")
  ;;   (let ((dir (if (dir-name)
  ;;                  (directory-file-name dir-name)
  ;;                  (default-directory)))))
  ;;   (async-start-process
  ;;    "ctags"
  ;;    "ctags" "-f" "TAGS" "-e" "-R" dir))

  ;; (defun create-project-tags (directory)
  ;;     "Create TAGS file for current project."
  ;;     (interactive "DSource Directory:")
  ;;     (let ((default-directory (project-root (project-current t))))
  ;;       (async-start-process "ctags" "ctags" nil "-f" "TAGS" "-e" "-R" (file-relative-name directory default-directory))))

  ;; (defun import-external-to-project-tags (external-tags-file)
  ;;   "Import other TAGS file."
  ;;   (interactive "fTAGS file:")
  ;;   (let ((default-directory (project-root (project-current t))))
  ;;     (async-start-process "ctags" "ctags" nil "-f" "TAGS" "--append" "-e" (s-concat "--etags-include=" external-tags-file))))

  ;; ;; (defun update-tags-for-file-in-project ()
  ;; ;;   "Update TAGS file for this file"
  ;; ;;   (let* ((default-directory (project-root (project-current t)))
  ;; ;;         (file (file-relative-name (buffer-file-name (current-buffer)) default-directory)))
  ;; ;;     (async-start-process "ctags" "ctags" nil "-f" "TAGS" "-e" "--append" file)))

  ;; (define-minor-mode tags-update-mode
  ;;   nil
  ;;   :lighter tags
  ;;   (let ((refresh-function (if (project-current)
  ;;                               #'update-tags-for-file-in-project
  ;;                               #'create-tags)))
  ;;     (if tags-update-mode
  ;;       (add-hook 'after-save-hook refresh-function 0 t)
  ;;       (remove-hook 'after-save-hook refresh-function t))))

  ;; (add-hook 'zig-mode-hook #'tags-update-mode)

  ;; (use-package etags
  ;;   :config
  ;;   (setq tags-revert-without-query t))

  (use-package citre
    :config
    (setq citre-use-project-root-when-creating-tags t
          citre-auto-enable-citre-mode-modes '(prog-mode)
          citre-capf-substr-completion t)
    :hook (prog-mode . citre-mode))
  (use-package citre-config)

  (use-package eldoc
    :config
    (setq lsp-eldoc-render-all 't
          eldoc-documentation-strategy #'eldoc-documentation-compose
          eldoc-echo-area-use-multiline-p 't
          eldoc-idle-delay 0.75))

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

  (defun project-compilation-buffer-name (name-of-mode)
    (concat "*" (project-root (project-current)) " - " (downcase name-of-mode) "*"))
  (setq project-compilation-buffer-name-function #'project-compilation-buffer-name)

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
  (use-package guix
    :config
    (global-guix-prettify-mode))

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

;; Dotnet

;; (use-package fsharp-mode

;;     :config
;;     (setq-default fsharp-indent-offset 2))

;;; Apl

  ;; (use-package gnu-apl-mode
  ;;   :config
  ;;   (setq gnu-apl-keymap-template "
  ;; ╔════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦═════════╗
  ;; ║ ±∇ ║ !∇ ║ @∇ ║ #∇ ║ $∇ ║ %∇ ║ ^∇ ║ &∇ ║ *∇ ║ (∇ ║ )∇ ║ _∇ ║ +∇ ║         ║
  ;; ║ §∇ ║ 1∇ ║ 2∇ ║ 3∇ ║ 4∇ ║ 5∇ ║ 6∇ ║ 7∇ ║ 8∇ ║ 9∇ ║ 0∇ ║ -∇ ║ =∇ ║ BACKSP  ║
  ;; ╠════╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦══════╣
  ;; ║       ║ \"∇ ║ <∇ ║ >∇ ║ P∇ ║ Y∇ ║ F∇ ║ G∇ ║ C∇ ║ R∇ ║ L∇ ║ ?∇ ║ +∇ ║ RET  ║
  ;; ║  TAB  ║ '∇ ║ ,∇ ║ .∇ ║ p∇ ║ y∇ ║ f∇ ║ g∇ ║ c∇ ║ r∇ ║ l∇ ║ /∇ ║ =∇ ║      ║
  ;; ╠═══════╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╗    ║
  ;; ║ (CAPS   ║ A∇ ║ O∇ ║ E∇ ║ U∇ ║ I∇ ║ D∇ ║ H∇ ║ T∇ ║ N∇ ║ S∇ ║ _∇ ║ |∇ ║    ║
  ;; ║  LOCK)  ║ a∇ ║ o∇ ║ e∇ ║ u∇ ║ i∇ ║ d∇ ║ h∇ ║ t∇ ║ n∇ ║ s∇ ║ -∇ ║ \\∇ ║    ║
  ;; ╠════════╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩════╩════╣
  ;; ║        ║ ~∇ ║ Z∇ ║ X∇ ║ C∇ ║ V∇ ║ B∇ ║ N∇ ║ M∇ ║ <∇ ║ >∇ ║ ?∇ ║          ║
  ;; ║  SHIFT ║ `∇ ║ z∇ ║ x∇ ║ c∇ ║ v∇ ║ b∇ ║ n∇ ║ m∇ ║ ,∇ ║ .∇ ║ /∇ ║  SHIFT   ║
  ;; ╚════════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩══════════╝"))

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
    (setq org-preview-latex-default-process 'dvisvgm
          org-latex-compiler "xelatex")
    ;; Org latex preview
    (setq org-preview-latex-process-alist
          '((dvipng
             :programs ("xelatex" "dvipng")
             :description "xdv > png"
             :message "you need to install the programs: xelatex and dvipng."
             :image-input-type "xdv"
             :image-output-type "png"
             :image-size-adjust (1.0 . 1.0)
             :latex-compiler ("xelatex -no-pdf -shell-escape -interaction nonstopmode -output-directory %o %f")
             :image-converter ("dvipng -D %D -T tight -o %O %f")
             :transparent-image-converter
             ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
            (dvisvgm
             :programs ("xelatex" "dvisvgm")
             :description "xdv > svg"
             :message "you need to install the programs: xelatex and dvisvgm."
             :image-input-type "xdv"
             :image-output-type "svg"
             :image-size-adjust (1.7 . 1.5)
             :latex-compiler ("xelatex -no-pdf -shell-escape -interaction nonstopmode -output-directory %o %f")
             :image-converter ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))
            (imagemagick
             :programs ("xelatex" "convert")
             :description "pdf > png"
             :message "you need to install the programs: xelatex and imagemagick."
             :image-input-type "pdf"
             :image-output-type "png"
             :image-size-adjust (1.0 . 1.0)
             :latex-compiler ("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")
             :image-converter
             ("convert -density %D -trim -antialias %f -quality 100 %O"))))
    (let ((texcmd))
      (setq texcmd "latexmk -pdflatex='xelatex -file-line-error --shell-escape -synctex=1' -pdf %f")
      (setq org-latex-default-packages-alist
            '(("" "fontspec" t)
              ("" "xunicode" t)
              ("" "url" t)
              ;;            ("" "minted" t)
              ;; ("" "rotating" t)
              ;; ("" "memoir-article-styles" t)
              ;; ("american" "babel" t)
              ;; ("babel" "csquotes" t)
              ;; ("" "listings" nil)
              ("svgnames" "xcolor" t)
              ("" "amsmath" t)
              ("" "amssymb" t)
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
                               '((coq . t)
                                 (julia . t)))

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

  (use-package zig-mode
    :config
    (setq zig-format-show-buffer nil))
  ;;        zig-indent-offset 2))

  ;; (compilation-buffer-name-function
  ;;  (or project-compilation-buffer-name-function
  ;;      compilation-buffer-name-function))
  ;; (name-of-mode "compilation")
  ;; (compilation-buffer
  ;;  (get-buffer-create
  ;;   (compilation-buffer-name name-of-mode nil compilation-buffer-name-function)))

  (defun cf/get-compilation-buffer-name ()
    (let ((compilation-buffer-name-function
           (or project-compilation-buffer-name-function
               compilation-buffer-name-function))
          (name-of-mode "compilation"))
      (compilation-buffer-name name-of-mode nil compilation-buffer-name-function)))

  (defvar cf/compilation-buffer-buffer-update-alist '()
    "Association list between compilation buffers and source buffers in which the error should be reported")
  (defvar-local cf/compilation-error-list '())

  (defun cf/update-compilation-error-list (compilation-buffer source-buffer &rest _)
    (if  (memq 'cf-mode (buffer-local-value 'local-minor-modes (current-buffer)))
        (let* ((source-directory (project-root (project-current t)))
               (current-file-absolute (buffer-file-name source-buffer))
               (current-file (file-relative-name current-file-absolute source-directory)))
          (with-current-buffer source-buffer
            (setq cf/compilation-error-list '()))
          (with-current-buffer compilation-buffer
            (goto-char (point-min))
            (while (search-forward-regexp
                    "^\\(.*.zig\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\)$" nil t)
              (let* ((msg (match-string 4))
                     (file (match-string 1))
                     (line (string-to-number (match-string 2)))
                     (col (string-to-number (match-string 3)))
                     ;; Zig does not warn, only errors or notes.
                     (type (if (string-match "^error" msg)
                               :error
                             :note)))
                (if (or (string-equal file current-file)
                        (string-equal file current-file-absolute))
                    (let* ((region (flymake-diag-region
                                    source-buffer
                                    line
                                    col))
                           (beg (car region))
                           (end (cdr region)))
                      (with-current-buffer source-buffer
                        (save-excursion
                          (setq beg-m (make-marker))
                          (set-marker beg-m beg source-buffer)
                          (setq end-m (make-marker))
                          (set-marker end-m end source-buffer)
                          (setq cf/compilation-error-list
                                (cons
                                 `((buffer . ,source-buffer)
                                   (beg-m . ,beg-m)
                                   (end-m . ,end-m)
                                   (type . ,type)
                                   (msg . ,msg))
                                 cf/compilation-error-list)))))))))
          ;; call flymake start to force it to recompute errors
          (with-current-buffer source-buffer
            (flymake-start)))))
  (defun cf/flymake-backend (report-fn &rest _args)
    (funcall report-fn
             (mapcar
              (lambda (error)
                (let ((buffer (cdr (assoc 'buffer error)))
                      (beg (cdr (assoc 'beg-m error)))
                      (end (cdr (assoc 'end-m error)))
                      (type (cdr (assoc 'type error)))
                      (msg (cdr (assoc 'msg error))))
                  (flymake-make-diagnostic
                   buffer
                   beg
                   end
                   type
                   msg)))
              cf/compilation-error-list)
             :region (cons (point-min) (point-max))))

  (defun cf/compilation-finish (compilation-buffer _result)
    (let ((compilation-buffer-name (buffer-name compilation-buffer)))
      (mapc (lambda (pair)
              (if (equal (car pair) compilation-buffer-name)
                  (cf/update-compilation-error-list compilation-buffer (cdr pair))))
            cf/compilation-buffer-buffer-update-alist)))
  (add-hook 'compilation-finish-functions #'cf/compilation-finish)

  (defun cf/remove-buffer-from-alist ()
    (let* ((buffer (current-buffer))
           (minors (buffer-local-value 'local-minor-modes buffer)))
      (memq 'cf-mode (buffer-local-value 'local-minor-modes (current-buffer)))
      (if (memq 'cf-mode minors)
          (setq cf/compilation-buffer-buffer-update-alist
                (cl-remove buffer
                           cf/compilation-buffer-buffer-update-alist
                           :test (lambda (a b)
                                   ;; delete if buffer is either dead or current buffer
                                   (if (buffer-live-p (cdr b))
                                       (string-equal (buffer-name a) (buffer-name (cdr b)))
                                     t)))))))

  (define-minor-mode cf-mode
    nil
    :lighter cf
    (if cf-mode
        (let ((compilation-buffer-n (cf/get-compilation-buffer-name)))
          (add-hook 'flymake-diagnostic-functions #'cf/flymake-backend nil t)
          (add-hook 'kill-buffer-hook #'cf/remove-buffer-from-alist)
          (add-to-list 'cf/compilation-buffer-buffer-update-alist
                       (cons compilation-buffer-n (current-buffer))))
      (progn
        (remove-hook 'flymake-diagnostic-functions #'cf/flymake-backend t)
        (cf/remove-buffer-from-alist))))


  (add-hook 'zig-mode-hook 'flymake-mode)
  (add-hook 'zig-mode-hook 'cf-mode)

  (defvar zig-flymake-command '("zig" "build" "test"))
  ;; Variable saving if any flymake process is running.
  (defvar zig--flymake-proc nil)
  (defun zig-flymake (report-fn &rest _args)
    "Zig flymake command, called by flymake-start."
    (unless (executable-find "zig")
      (error "Cannot find suitable zig executable"))
    ;; Kill the last zig--flymake-proc, as flymake can spawn them indefinitely.
    (when (process-live-p zig--flymake-proc)
      (kill-process zig--flymake-proc))

    (save-restriction
      (widen)
      (setq
       zig--flymake-proc
       (make-process
        :name "zig-flymake" :noquery t :connection-type 'pipe
        :buffer (generate-new-buffer "*zig-flymake*")
        :command zig-flymake-command
        :sentinel
        (lambda (proc _event)
          (when (memq (process-status proc) '(exit signal))
            (unwind-protect
                (if (eq proc zig--flymake-proc)
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-min))
                      (let ((diags '()))
                        (while (search-forward-regexp
                                "^\\(.*.zig\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\)$" nil t)
                          (let* ((msg (match-string 4))
                                 (file (match-string 1))
                                 (beg-line (string-to-number (match-string 2)))
                                 (beg-col (string-to-number (match-string 3)))
                                 ;; Zig does not warn, only errors or notes.
                                 (type (if (string-match "^error" msg)
                                           :error
                                         :note))
                                 (diag (flymake-make-diagnostic
                                        file
                                        (cons beg-line beg-col)
                                        ;; Errors will have properly recalculated ends
                                        ;; as when flymake-make-diagnostic gots file as
                                        ;; locus, it automatically calls flymake-diag-region
                                        ;; when trying to convert line and col into beg end.
                                        nil
                                        type
                                        msg)))
                            (setq diags (cons diag diags))))
                        (funcall report-fn diags
                                 ;; If the buffer hasn't changed since last
                                 ;; call to the report function, flymake won't
                                 ;; delete old diagnostics.  Using :region
                                 ;; keyword forces flymake to delete
                                 ;; them (github#159).
                                 :region (cons (point-min) (point-max)))))
                  ;;(setq (append diags flymake-list-only-diagnostics))))
                  (flymake-log :warning "Canceling obsolete check %s"
                               proc))
              (kill-buffer (process-buffer proc)))))))))

  (defun zig--setup-flymake-backend ()
    (add-hook 'flymake-diagnostic-functions 'zig-flymake nil t))

  ;;(add-hook 'zig-mode-hook 'zig--setup-flymake-backend)
  ;;(add-hook 'zig-mode-hook 'flymake-mode)
