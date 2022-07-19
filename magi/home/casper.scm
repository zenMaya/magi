(define-module (magi home casper)
  #:use-module (magi home packages)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (guix gexp))

(home-environment
 (packages `(
	     ,@fonts
	     ,@sway-desktop
	     ,@utilities
	     ,@browsers
	     ,@cc-toolchain
	     ,@avr-toolchain
	     ,@haskell-toolchain
             ,@racket-toolchain
	     ,@guile-toolchain
	     ,@python-toolchain
             ,@coq-toolchain
	     ,syncthing-package))
 (services
  `(
    ,@emacs-services
    ,@sway-services
    ,@configuration-services
  ; ,syncthing-services
    ,(service home-fish-service-type
	      (home-fish-configuration
	       (environment-variables '(
					("PATH" . "$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"))))))))
