(define-module (magi system casper)
  #:use-module (magi system)
  #:use-module (gnu)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
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
		      (keyboard-layout dvorak-ucw))))
		   nonguix-desktop-services))
 (packages (append (list
		    xf86-input-libinput)
		   (operating-system-packages magi)))
 (file-systems
  (cons*
   (file-system
    (device (file-system-label "BOOT"))
    (mount-point "/boot/efi")
    (type "vfat"))
   (file-system
    (device (file-system-label "system"))
    (mount-point "/")
    (type "btrfs"))
   %base-file-systems)))
