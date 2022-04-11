(define-module (magi home casper)
  #:use-module (magi home packages)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services))

(home-environment
 (packages `(
	     ,@browsers
	     ,@haskell-toolchain
	     ,@zig-toolchain
	     ,@rust-toolchain
	     ,@guile-toolchain
	     ,@emacs-packages
	     ))
 (services
  (list
   (service home-fish-service-type
	    (home-fish-configuration)))))
