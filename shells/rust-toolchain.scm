(define-module (shells rust-toolchain)
  #:use-module (gnu packages))

(specifications->manifest
       (list
	"rust"
	"rust-analyzer"))
