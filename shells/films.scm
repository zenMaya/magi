 (define-module (shells films)
 #:use-module (guix packages)
 #:use-module (guix download)
 #:use-module (guix build-system python)
 #:use-module ((guix licenses) #:prefix license:)
 #:use-module (gnu packages)
 #:use-module (gnu packages xml)
 #:use-module (gnu packages python-xyz)
 #:use-module (gnu packages python-web)
 #:use-module (gnu packages machine-learning)
 #:use-module (gnu packages tor))



(define-public python-ulozto-downloader
  (package
    (name "python-ulozto-downloader")
    (version "3.3.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "ulozto-downloader" version))
              (sha256
               (base32
                "0jjmcz0qh48jii8jzshn0fmb09wglc6r9sgbm6d3s4x1f56mvmxv"))))
    (build-system python-build-system)
    (propagated-inputs (list python-ansicolors
                             python-numpy
                             python-pillow
                             python-pysocks
                             python-requests
                             python-stem))
    (home-page "https://github.com/setnicka/ulozto-downloader")
    (synopsis "Uloz.to quick multiple sessions downloader.")
    (description "Uloz.to quick multiple sessions downloader.")
    (license license:expat)))


 (packages->manifest (list
                     python-ulozto-downloader
                     tensorflow-lite
                     tor))
