(define-module (magi system casper)
  #:use-module (magi system)
  #:use-module (gnu)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu services syncthing)
  #:use-module (gnu services docker)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gnome)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

(operating-system
 (inherit magi)
 (host-name "casper")
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))
 (keyboard-layout dvorak-ucw)
 (services (append (list
		    (service gnome-desktop-service-type)
		    (set-xorg-configuration
		     (xorg-configuration
		      (keyboard-layout dvorak-ucw)))
		    (bluetooth-service #:auto-enable? #t)
		    (service docker-service-type
			     (docker-configuration))
		    (service syncthing-service-type
			     (syncthing-configuration
			      (user "maya")
			      (arguments '("--no-browser" "--no-default-folder" "--log-max-size=1000")))))
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
		  "shadow"))))
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
