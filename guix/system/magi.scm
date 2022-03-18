(define-module (magi)
  #:use-module (gnu)
  #:use-module (gnu services networking ssh)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux))
 
(define %xorg-libinput-config
  "Section \"InputClass\"
  Identifier \"Touchpads\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsTouchpad \"on\"

  Option \"Tapping\" \"on\"
  Option \"TappingDrag\" \"on\"
  Option \"DisableWhileTyping\" \"on\"
  Option \"MiddleEmulation\" \"on\"
  Option \"ScrollMethod\" \"twofinger\"
EndSection
Section \"InputClass\"
  Identifier \"Keyboards\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsKeyboard \"on\"
EndSection
")

(define-public magi-os
  (operating-system
   (host-name "magi")
   (timezone "Europe/Prague")
   (locale "en_US.utf8")
   (keyboard-layout
    (keyboard-layout "cz" "dvorak-ucw" #:options '("ctrl:nocaps")))
    (bootloader (bootloader-configuration
		 (bootloader grub-efi-bootloader)
		 (target "/boot/efi")
		 (keyboard-layout keyboard-layout)))
    (file-systems (cons*
		  (file-system
		   (mount-point "/tmp")
		   (device "none")
		   (type "tmpfs")
		   (check? #f))
		  %base-file-systems))
    (users (cons (user-account
		  (name "maya")
		  (comment "Mája")
		  (group "users")
		  (home-directory "/home/maya")
		  (supplementary-groups '(
					  "wheel"
					  "tty"
					  "input"
					  "lp"
					  "audio"
					  "video")))
		 %base-user-accounts))
    (packages (append (list
		       git
		       ntfs-3g
		       fuse-exfat
		       emacs)
		      %base-packages))
    (services (cons* (service openssh-service-type)))))
