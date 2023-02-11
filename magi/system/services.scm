(define-module (magi system services)
  #:use-module (magi system docker)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu system pam)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services pm)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu services authentication)
  #:use-module (gnu services security-token)
  #:use-module (gnu packages freedesktop))

(define-record-type* <fprintd-pam-configuration>
  fprintd-pam-configuration make-fprintd-pam-configuration
  fprintd-pam-configuration?
  (package fprintd-pam-configuration-package (default fprintd))
  (pam-services fprintd-pam-configuration-pam-services (default (list "polkit-1"))))

(define (fprintd-pam-pam-services config)
  (let ((fprintd-module
         #~(string-append #$(fprintd-pam-configuration-package config) "/lib/security/pam_fprintd.so")))
    (list
     (lambda (pam)
       (if (member (pam-service-name pam)
                   (fprintd-pam-configuration-pam-services config))
           (let ((sufficient
                  (pam-entry
                   (control "sufficient")
                   (module fprintd-module))))
             (pam-service
              (inherit pam)
              (auth (cons sufficient (pam-service-auth pam)))))
           pam))
     (pam-service
      (inherit (unix-pam-service "gdm-fingerprint"
                                 #:login-uid? #t))
      (auth (list
             (pam-entry
              (control "required")
              (module fprintd-module))))))))

(define fprintd-pam-service-type
  (service-type (name 'fprintd-pam)
                (description "")
                (extensions
                 (list
                  (service-extension pam-root-service-type fprintd-pam-pam-services)))))


(define-public magi-services
  (list
   ;; (simple-service 'disable-gdm-sleep etc-service-type
   ;;                 `(("")))
   (service guix-publish-service-type
            (guix-publish-configuration
             (port 420)
             (advertise? #t)))))

(define-public laptop-services
  (list
   (service tlp-service-type
            (tlp-configuration))
   (service fprintd-service-type)
   (service fprintd-pam-service-type
            (fprintd-pam-configuration))
   (service pcscd-service-type)))

(define-public jellyfin-service
  (simple-service 'jellyfin-service docker-service-type
                  (list
                   (docker-container
                    (name 'docker-jellyfin)
                    (documentation "Jellyfin service.")
                    (image-name "jellyfin/jellyfin:latest")
                    (volumes '(("/media/huge/jellyfin" "/opt/jellyfin")
                               ("/media/huge" "/media/big")))
                    (network "host")
                    (additional-arguments '("--user=114:120"
                                            "-e" "JELLYFIN_CACHE_DIR=/opt/jellyfin/cache"
                                            "-e" "JELLYFIN_CONFIG_DIR=/opt/jellyfin/config"
                                            "-e" "JELLYFIN_DATA_DIR=/opt/jellyfin/data"
                                            "-e" "JELLYFIN_LOG_DIR=/opt/jellyfin/log"))))))
