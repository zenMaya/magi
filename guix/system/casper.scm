(define-module (casper)
  #:use-module (magi)
  #:use-module (gnu)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gnome)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

(operating-system
 (inherit magi-os)
 (host-name "casper")
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))
 (services (append (append (list
		    (service gnome-desktop-service-type)
		    (set-xorg-configuration
		     (xorg-configuration
		      (keyboard-layout keyboard-layout))))
		   (operating-system-services magi)) %desktop-services))
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
