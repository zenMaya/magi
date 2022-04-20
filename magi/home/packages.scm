(define-module (magi home packages)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs)
  #:use-module (rde packages emacs)
  #:use-module (gnu home-services emacs)
  ;;; for custom packages
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages))

(define-public emacs-nano-theme
  (package
   (name "emacs-nano-theme")
   (version "0.3.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://elpa.gnu.org/packages/nano-theme-"
           version
           ".tar"))
     (sha256
      (base32 "1nq5x46467vnsfg3fzb0qyg97xpnwsvbqg8frdjil5zq5fhsgmrz"))))
   (build-system emacs-build-system)
   (home-page "https://github.com/rougier/nano-theme")
   (synopsis "N Λ N O theme")
   (description
    "N Λ N O theme is a consistent theme that comes in two flavors: 
@itemize
@item a light theme that is based on Material (@url{https://material.io/})
@item a dark theme that is based on Nord (@url{https://www.nordtheme.com/})
@end itemize

A theme is fully defined by a set of (1+6) faces as explained in this paper
@url{https://arxiv.org/abs/2008.06030}

Recommended font is \"Roboto Mono\" (package @code{fonts-google-roboto}) or \"Roboto Mono Nerd\" if you want to benefit
from all the fancy glyphs.")
   (license license:gpl3+)))

(define-public emacs-nano-agenda
  (package
   (name "emacs-nano-agenda")
   (version "0.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://elpa.gnu.org/packages/nano-agenda-"
           version
           ".tar"))
     (sha256
      (base32 "0j29fwc273mjdlj83h1a46sb7z3j066qqnp2i78kn2pmgjg27szb"))))
   (build-system emacs-build-system)
   (home-page "https://github.com/rougier/nano-agenda")
   (synopsis "N Λ N O agenda")
   (description
    "N Λ N O agenda is a minimal view of your org agenda files.  It displays a
calendar view of current month (or the month corresponding to the current
selected date) alongside a view of your agenda displaying timestamped entries.
The agenda can be navigated using arrows keys and killed using \"q\", \"return\" or
\"escape\".")
   (license license:gpl3+)))

(define-public universal-media-server
  (package
   (name "universal-media-server")
   (version "10.19.0")
   (source
    (origin
     (methon )))))

(define-public fonts
  (map specification->package
       (list
	"font-fira-code"
	"font-google-roboto")))

(define-public utilities
  (map specification->package+output
       (list
	"git:send-email")))

(define-public browsers
  (map specification->package
       (list
	"icecat"
	"firefox")))

(define-public haskell-toolchain
  (map specification->package
       (list
	"cabal-install"
	"ghc"
	"hlint"
	"hoogle"
	"stylish-haskell")))

(define-public zig-toolchain
  (map specification->package
       (list
	"zig")))

(define-public rust-toolchain
  (map specification->package
       (list
	"rust"
	"rust-analyzer")))

(define-public guile-toolchain
  (map specification->package
       (list
	"guile"
	"guile-colorized"
	"guile-readline"
	"guile-syntax-highlight")))

(define-public emacs-packages
   (append (map specification->package
	 (list
;;; global
	  "emacs-use-package"
	  "emacs-orderless"
	  "emacs-vertico"
	  "emacs-marginalia"
;;; visual
	  "emacs-nano-modeline"
					;	"emacs-consult"
	  "emacs-embark"
;;; languages
	  "emacs-corfu"
	  "emacs-cape"
	  "emacs-eglot"
;;; lisps
	  "emacs-geiser"
	  "emacs-geiser-guile"
	  "emacs-paredit")) (list emacs-nano-agenda emacs-nano-theme)))

(define-public emacs-services
  (list
   (simple-service 'emacs-init
		   home-files-service-type
		   `((".config/emacs/early-init.el" ,(local-file "../../configuration/emacs/early-init.el"))
		     (".config/emacs/init.el" ,(local-file "../../configuration/emacs/init.el"))))
   (service home-emacs-service-type
	    (home-emacs-configuration
	     (package emacs-next-pgtk)
	     (elisp-packages emacs-packages)))))
