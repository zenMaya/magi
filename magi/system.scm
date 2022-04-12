(define-module (magi system)
  #:use-module (gnu)
  #:use-module (gnu services desktop)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages certs)
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

(define-public dvorak-ucw (keyboard-layout "cz" "dvorak-ucw" #:options '("ctrl:nocaps")))

(define-public nonguix-desktop-services (modify-services %desktop-services
             (guix-service-type config => (guix-configuration
               (inherit config)
               (substitute-urls
                (append (list "https://substitutes.nonguix.org")
                  %default-substitute-urls))
               (authorized-keys
                (append (list (plain-file "non-guix.pub" "(public-key 
 (ecc 
  (curve Ed25519)
  (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)
  )
 )"))
                  %default-authorized-guix-keys))))))

(define-public magi
  (operating-system
   (host-name "magi")
   (timezone "Europe/Prague")
   (locale "en_US.utf8")
   (keyboard-layout dvorak-ucw)
    (bootloader (bootloader-configuration
		 (bootloader grub-efi-bootloader)
		 (target "/boot/efi")
		 (keyboard-layout keyboard-layout)))
    (file-systems (cons*
		  (file-system
		   (mount-point "/")
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
               nss-certs
		       ntfs-3g
		       fuse-exfat
		       emacs)
		      %base-packages))
    (services %base-services)))
