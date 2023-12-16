(define-module (magi home emacs)
  #:use-module (magi home packages)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (rde home services emacs)
  #:use-module (rde gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs))

(define emacs-packages
  (append (map specification->package
               (list
;;; global
                "emacs-use-package"
                "emacs-disable-mouse"
                "emacs-persp-mode"
                "emacs-orderless"
                "emacs-vertico"
                "emacs-consult"
                "emacs-marginalia"
                "emacs-which-key"
                "emacs-helpful"
                "emacs-ace-jump-mode"
                "emacs-embark"
                "emacs-sr-speedbar"
                "emacs-popwin"
                "emacs-multiple-cursors"
;;; visual
                "emacs-nano-modeline"
                "emacs-ef-themes"
                "emacs-ligature"
;;; apps
                "emacs-vterm"
                "emacs-multi-vterm"
                "emacs-guix"
                "emacs-pinentry"
                "emacs-pass"
                "emacs-notmuch"
                ;;                "emacs-calibredb"
;;; languages
                "emacs-corfu"
                "emacs-cape"
                "emacs-kind-icon"
                ;;"emacs-company"
                "emacs-eglot"
                ;;"emacs-lsp-mode"
                ;;"emacs-lsp-ui"
                "emacs-eldoc"
                "emacs-eldoc-box"
                "emacs-citre"
;;              "emacs-tramp"
;;              "emacs-docker-tramp"
                "emacs-smartparens"
                "emacs-polymode"
                "emacs-hideshowvis"
                "emacs-yasnippet"
;;; markdown
                "emacs-markdown-mode"
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
; too old       "emacs-zig-mode"
;;; lisp
;               "emacs-geiser"
;               "emacs-geiser-guile"
;               "emacs-geiser-racket"
                "emacs-racket-mode"
                "emacs-scribble-mode"
                "emacs-sly"
;;              "emacs-paredit"
                "emacs-haskell-mode"
;;; prolog
;;; ocaml
                "emacs-tuareg"
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
;;; dotnet
;;; jvm
                "emacs-lsp-java"
                "emacs-scala-mode"
                "emacs-clojure-mode"
                "emacs-cider"))
          (list
           emacs-bufler emacs-zig-mode
           emacs-svelte-mode emacs-parinfer-rust-mode ;; emacs-fsharp-mode emacs-eglot-fsharp
           emacs-eglot-java)))
;emacs-embark-consult emacs-spotify
(define-public emacs-services
  (list
   ;; (simple-service 'emacs-init
   ;;                 home-files-service-type
   ;;                 `((".config/emacs/early-init.el" ,(local-file "../../configuration/emacs/early-init.el"))))
   ;;                  (".config/emacs/init.el" ,(local-file "../../configuration/emacs/init.el"))))
   (service home-emacs-service-type
            (home-emacs-configuration
             (emacs emacs-pgtk)
	     (native-comp? #t)
             (xdg-flavor? #t)
             (elisp-packages emacs-packages)
             (early-init-el
              `(,(slurp-file-like (local-file "../../configuration/emacs/early-init.el"))))
             (init-el
              `(,(slurp-file-like (local-file "../../configuration/emacs/init.el"))))))))
