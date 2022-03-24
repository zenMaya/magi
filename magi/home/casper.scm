(define-module (magi home casper)
  #:use-module (magi home devel)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages ssh))

(home-environment
 (packages `(
	     ,@development-packages
	     ,openssh))
 (services
  (list
   (service home-fish-service-type
	    (home-fish-configuration)))))
