(define-module (magi)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (guix records)
  #:use-module (gnu home)
  #:use-module (gnu system)
  #:use-module (gnu system shadow)
  #:use-module (gnu services configuration)
  #:export
  (dispatcher
   magi-config
   magi-user-config))

(define (alist? lst)
  (every pair? lst))

(define (services-getter? fn)
  (procedure? fn))

(define-record-type* <magi-user-config> magi-user-config
  make-magi-user-config
  magi-user-config?
  this-magi-user-config

  (name magi-user-config-name (default #f))
  (account magi-user-config-account (default '()))
  (environment
   magi-user-config-environment
   (default '())))

(define-record-type* <magi-config> magi-config
  make-magi-config
  magi-config?
  this-magi-config

  (name magi-config-name
        (default #f))
  (users magi-config-users
p         (default '()))
  (os
   magi-config-os
   (default '())))

(define (magi-config->operating-system config)
  (let* ((os (magi-config-os config))
         (users (append (map magi-user-config-account (magi-config-users config))
                        (operating-system-users os)
                        %base-user-accounts)))
    (operating-system
     (inherit os)
     (users users))))

(define (find-config name configs)
  (match configs
    (() (display "Config not found " name))
    ((head . tail) (if (equal? (magi-config-name head) name)
                       head
                       (find-config name tail)))))

(define (find-user-config name configs)
  (match configs
    (() (display "User config not found"))
    ((head . tail) (if (equal? (magi-user-config-name head) name)
                       head
                       (find-user-config name tail)))))

(define-public (dispatcher configs)
  (let* ((magi-target (getenv "MAGI_TARGET"))
         (magi-target-list (string-split magi-target #\-)))
    (pretty-print magi-target-list)
    (match (car magi-target-list)
      ("home" (magi-user-config-environment
               (find-user-config
                (cadr magi-target-list)
                (magi-config-users
                 (find-config (caddr magi-target-list) configs)))))
      ("system" (magi-config->operating-system
                 (find-config (cadr magi-target-list) configs))))))
