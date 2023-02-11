(define-module (shells zig-toolchain))

(use-modules
 (ice-9 ftw)
 (ice-9 match)
 (ice-9 regex)
 (ice-9 format)
 (ice-9 ftw)
 (gnu packages)
 (gnu packages zig)
 (gnu packages base)
 (gnu packages curl)
 (gnu packages python)
 (gnu packages compression)
 (guix gexp)
 (guix packages)
 (guix profiles)
 (guix licenses)
 (guix git-download)
 (guix build utils)
 (guix build-system gnu)
 (guix build-system trivial))

(define zig-zls
  (package
    (name "zig-zls")
    (version "0.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zigtools/zls")
                    (commit version)
                    (recursive? #t)))
              (sha256
               (base32 "1hhs7dz9rpshfd1a7x5swmix2rmh53vsqskh3mzqlrj2lgb3cnii"))))
    (build-system gnu-build-system)
    (inputs (list zig python))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out"))
                    (zig (string-append #$zig "/bin/zig")))
                (setenv "ZIG_GLOBAL_CACHE_DIR"
                        (string-append (getcwd) "/zig-cache"))
                (invoke zig "build" "install" "-Drelease-safe" "--prefix" out))))
          (delete 'install)
          (replace 'check
            (lambda _
              (let ((zig (string-append #$zig "/bin/zig")))
                (invoke zig "build" "test")))))))
    (synopsis "Zig language server")
    (description "Zig Language Server is a language server for the @code{zig} programming language.")
    (home-page "https://github.com/zigtools/zls")
    (license expat)))

(packages->manifest
 (list
  zig
  zig-zls))

  ;;    (arguments
;;       (list
;;        #:modules '((guix build utils))
;;        #:builder
;; ))
       ;; #~(begin
       ;;     (use-modules (guix build utils))
       ;;     (let* ((in #$source)
       ;;            (tar (string-append #$tar "/bin/tar"))
       ;;            (out (string-append #$output "/out"))
       ;;            (zig (string-append #$zig "/bin/zig"))
       ;;            (version #$version))
       ;;       (mkdir-p "build")
       ;;       (setenv "PATH" (string-append (getenv "PATH") ":" #$gzip "/bin/"))
       ;;       (invoke tar "xvf" in "-C" "build")
       ;;       (chdir "build")
       ;;       (chdir (string-append "zls-" version))
       ;;       (invoke zig "build" "-Drelease-safe")
       ;;       (copy-recursively "./zig-out/" out)))
