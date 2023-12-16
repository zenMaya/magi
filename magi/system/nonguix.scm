(define-module (magi system nonguix)
  #:use-module (ice-9 match)
  #:use-module (magi system)
  #:use-module (guix gexp)
  #:use-module (gnu packages xorg)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:export (nonguix-desktop-services))

(define-public magi-nonguix
  (operating-system
   (inherit magi)
   (kernel linux)
   (initrd microcode-initrd)
   (firmware (list linux-firmware))))

(define* (nonguix-desktop-services base-desktop-services #:key (wayland? #f) (autologin #f))
  (modify-services base-desktop-services
                   (guix-service-type config => (guix-configuration
                                                 (inherit config)
                                                 (discover? #t)
                                                 (substitute-urls
                                                  (append (list "https://substitutes.nonguix.org")
                                                          %default-substitute-urls))
                                                 (authorized-keys
                                                  (cons (plain-file "nonguix-key.pub" "(public-key
 (ecc
  (curve Ed25519)
  (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)
  )
 )
")
                                                        %default-authorized-guix-keys))))
                   (gdm-service-type config => (gdm-configuration
                                                (inherit config)
                                                (wayland? wayland?)
                                                (xorg-configuration
                                                 (xorg-configuration
                                                  (server (if wayland?
                                                              xorg-server-xwayland
                                                              xorg-server))))
                                                ;; im keeping this for the later generations
                                                (auto-login? (if (equal? #f autologin) #f #t))
                                                (default-user autologin)))))
