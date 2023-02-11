(define-module (shells dotnet-toolchain)
  #:use-module (gnu packages))

(specifications->manifest
 (list "dotnet" "omnisharp"))
