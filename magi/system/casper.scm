(define-module (magi system casper)
  #:use-module (magi system)
  #:use-module (guix records)
  #:use-module (gnu)
  #:use-module (gnu system pam)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu services pm)
  #:use-module (gnu services syncthing)
  #:use-module (gnu services docker)
  #:use-module (gnu services authentication)
  #:use-module (gnu services security-token)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gnome)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

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

(operating-system
 (inherit (magi "casper"))
 (host-name "casper")
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))
 (keyboard-layout %cz-dvorak-ucw)
 (services (append (list
		    (service gnome-desktop-service-type)
		    (set-xorg-configuration
		     (xorg-configuration
		      (keyboard-layout %cz-dvorak-ucw)))
		    (bluetooth-service #:auto-enable? #t)
                    (service tlp-service-type
                             (tlp-configuration))
		    (service docker-service-type
			     (docker-configuration))
		    (service syncthing-service-type
			     (syncthing-configuration
			      (user "maya")
			      (arguments '("--no-browser" "--no-default-folder" "--log-max-size=1000"))))
                    (service fprintd-service-type)
                    (service fprintd-pam-service-type
                             (fprintd-pam-configuration))
                    (service pcscd-service-type))
		   (modify-services nonguix-desktop-services
				    (gdm-service-type config => (gdm-configuration
								 (inherit config)
								 (wayland? #t))))))
 (packages (append
	    (operating-system-packages magi)
	    (map specification->package
		 (list
		  "syncthing"
		  "sway"
		  "xf86-input-libinput"
		  "flatpak"
		  "fuse"
		  "podman"
		  "docker"
                  "runc"
		  "iptables"
		  "intel-media-driver"
		  "shadow"
		  "fprintd"
                  "opensc"))))
 (file-systems
  (cons*
   (file-system
    (device (file-system-label "BOOT"))
    (mount-point "/boot/efi")
    (type "vfat"))
   (file-system
    (device (file-system-label "guix"))
    (mount-point "/")
    (type "btrfs"))
   %base-file-systems)))
