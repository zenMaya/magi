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
	     ,@utilities
	     ,@browsers
	     ,@haskell-toolchain
	     ,@zig-toolchain
	     ,@rust-toolchain
	     ,@guile-toolchain
	     ))
 (services
  `(
    ,@emacs-services
    ,(service home-fish-service-type
	      (home-fish-configuration)))))
