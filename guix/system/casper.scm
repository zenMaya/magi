(define-module (casper)
  #:use-module (magi)
  #:use-module (gnu)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

(operating-system
 (inherit (magi))
 (host-name "casper")
 (kernel linux)
 (initrd microcode-initrd)
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
    (device (file-system-label "boot"))
    (mount-point "/boot/efi")
    (type "vfat"))
   (file-system
    (device (file-system-label "system"))
    (mount-point "/")
    (type "btrfs"))
   %base-file-systems)))