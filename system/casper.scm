(define-module (casper)
  #:use-module (magi)
  #:use-module (gnu)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

(operating-system
 (inherit (magi))
 (host-name "casper")
 (kernel linux)
 (firmware (list linux-firmware))
 (services (append (list
		    (service gnome-desktop-service-type)
		    (setx-xord-configuration
		     (xorg-configuration
		      (keyboard-layout keyboard-layout))))
		   (operating-system-services magi)))
 (packages (append (list
		    xf86-input-libinput)
		   (operating-system-packages magi)))
 (file-systems
  (cons*
   (file-system
    (device "/dev/sda1")
    (mount-point "/boot/efi")
    (type "vfat")
    (file-system-label "boot"))
   (file-system
    (device "/dev/sda2")
    (mount-point "/")
    (type "btrfs"))
   %base-file-systems)))
