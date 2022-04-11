(define-module (magi home packages)
  #:use-module (gnu packages))

(define-public browsers
  (map specification->package
       (list
	"icecat")))

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
  (map specification->package
       (list
	"emacs-geiser")))
