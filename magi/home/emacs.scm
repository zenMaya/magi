(define-module (magi home emacs)
  #:use-module (magi home packages)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (rde home services emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs))

(define emacs-packages
  (append (map specification->package
	       (list
;;; global
		"emacs-use-package"
                "emacs-disable-mouse"
                "emacs-perspective"
		"emacs-orderless"
		"emacs-vertico"
		"emacs-consult"
		"emacs-marginalia"
		"emacs-which-key"
                "emacs-helpful"
                "emacs-ace-jump-mode"
                "emacs-embark"
                "emacs-sr-speedbar"
;;; visual
		"emacs-nano-modeline"
                "emacs-ef-themes"
;;; apps
		"emacs-vterm"
		"emacs-guix"
                "emacs-pinentry"
                "emacs-pass"
                "emacs-notmuch"
                ;;                "emacs-calibredb"
;;; languages
		"emacs-corfu"
		"emacs-cape"
		"emacs-eglot"
                "emacs-eldoc"
;;		"emacs-tramp"
;;		"emacs-docker-tramp"
                "emacs-smartparens"
                "emacs-polymode"
                "emacs-hideshowvis"
;;; latex
                "emacs-cdlatex"
;;; web
		"emacs-web-mode"
		"emacs-typescript-mode"
;;; c/c++
                "emacs-bison-mode"
                "emacs-highlight-doxygen"
                "emacs-meson-mode"
;;; zig
;;; lisp
;		"emacs-geiser"
;		"emacs-geiser-guile"
;               "emacs-geiser-racket"
                "emacs-racket-mode"
                "emacs-sly"
;;		"emacs-paredit"
		"emacs-haskell-mode"
;;; prolog
;;; coq
                "proof-general"
                "emacs-company-coq"
;;; godot
                "emacs-gdscript-mode"
;;; org
		"emacs-org"
                "emacs-org-modern"
;;                "emacs-polymode-org"
                "emacs-org-contrib"
		"emacs-org-roam"
                "emacs-olivetti"
;;; markdown
		"emacs-markdown-mode"
;;; csharp
		"emacs-csharp-mode"
;;; jvm
                "emacs-scala-mode"
		))
	  (list emacs-bufler emacs-nano-agenda emacs-eldoc-box emacs-zig-mode emacs-flymake-swi-prolog emacs-svelte-mode emacs-maxima emacs-gnu-apl-mode emacs-parinfer-rust-mode)))
;emacs-embark-consult emacs-spotify
(define-public emacs-services
  (list
   (simple-service 'emacs-init
		   home-files-service-type
		   `((".config/emacs/early-init.el" ,(local-file "../../configuration/emacs/early-init.el"))
		     (".config/emacs/init.el" ,(local-file "../../configuration/emacs/init.el"))))
   (service home-emacs-service-type
	    (home-emacs-configuration
	     (package emacs-next)
	     (xdg-flavor? #t)
	     (elisp-packages emacs-packages)
             (emacs-servers '(server))))))
