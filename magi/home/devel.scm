(define-module (magi home devel)
  #:use-module (gnu packages)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages zig)
  #:use-module (gnu packages prolog))


(define-public development-packages
  (map specification->package
       (list
	"zig"
	"ghc"
	"swi-prolog")))
