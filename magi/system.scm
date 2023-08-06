(define-module (magi system)
  #:use-module (magi)
  #:use-module (gnu)
  #:use-module (gnu services desktop)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ssh))

(define-public %cz-dvorak-ucw (keyboard-layout "cz" "dvorak-ucw" #:options '("ctrl:nocaps")))

(define-public %grub-efi-bootloader
  (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets '("/boot/efi"))
                (keyboard-layout keyboard-layout)))

(define-public %magi-boot-file-system
  (file-system
    (device (file-system-label "BOOT"))
    (mount-point "/boot/efi")
    (type "vfat")))

(define-public %magi-root-file-system
  (file-system
    (device (file-system-label "magi"))
    (mount-point "/")
    (type "btrfs")
    (options "compress-force=zstd")))

(define-public magi
  (operating-system
   (host-name "")
   (timezone "Europe/Prague")
   (locale "en_US.utf8")
   (keyboard-layout %cz-dvorak-ucw)
   (name-service-switch %mdns-host-lookup-nss)
   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets '("/boot/efi"))
                (keyboard-layout %cz-dvorak-ucw)))
   (file-systems %base-file-systems)
   (users '())
   (groups (cons (user-group
                  (name "realtime")
                  (system? #t))
                 %base-groups))
   (packages (append (map specification->package+output
                          (list
                           "waypipe"
                           "nss-certs"
                           "ntfs-3g"
                           "dosfstools"
                           "fuse-exfat"
                           "emacs"
                           "btrfs-progs"
                           "openssh"
                           "make"
                           "mosh"))
                     %base-packages))
   (services %base-services)))
