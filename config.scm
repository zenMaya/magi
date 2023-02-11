(define-module (config)
  #:use-module (magi)
  #:use-module (magi system)
  #:use-module (magi system nonguix)
  #:use-module (magi system services)
  #:use-module (magi system web)
  #:use-module (magi system nvidia)
  #:use-module (magi system wireguard)
  #:use-module (magi home packages)
  ;; (magi home kodi)
  #:use-module (gnu packages kodi)
  #:use-module (magi home dbus)
  #:use-module (magi home emacs)
  #:use-module (magi home mail)
  #:use-module (magi home pipewire)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix transformations)
  #:use-module (gnu home)
  #:use-module (gnu bootloader)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (gnu system pam)
  #:use-module (gnu packages)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu services)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu services pm)
  #:use-module (gnu services nfs)
  #:use-module (gnu services vpn)
  #:use-module (gnu services ssh)
  #:use-module (gnu services web)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services avahi)
  #:use-module (gnu services spice)
  #:use-module (magi system docker)
  #:use-module (gnu services certbot)
  #:use-module (gnu services desktop)
  #:use-module (gnu services syncthing)
  #:use-module (gnu services networking)
  #:use-module (gnu services file-sharing)
  #:use-module (gnu services authentication)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services security-token)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (rde features mail)
  #:use-module (ice-9 match))

(define %dualsense-udev-rule
  (udev-rule
   "70-dualsense"
   "KERNEL==\"hidraw*\", ATTRS{idVendor}==\"054c\", ATTRS{idProduct}==\"0ce6\", MODE=\"0660\", TAG+=\"uaccess\"
KERNEL==\"hidraw*\", KERNELS==\"*054C:0CE6*\", MODE=\"0660\", TAG+=\"uaccess\""))

(define mail-accounts
  (list
   (mail-account
    (id 'disroot)
    (type 'disroot)
    (fqda "maya.tomasek@disroot.org"))))

(define mailing-lists
  (list
   (mailing-list
    (id 'guix-devel)
    (fqda "guix-devel@gnu.org")
    (config (l2md-repo
             (name "guix-devel")
             (urls "https://yhetil.org/guix-devel/0"))))))

(define* (maya-user-account #:key (supplementary-groups '()))
  (user-account
   (name "maya")
   (comment "Mája")
   (shell (file-append zsh "/bin/zsh"))
   (group "users")
   (home-directory "/home/maya")
   (supplementary-groups
    (cons*
     "wheel"
     "tty"
     "input"
     "lp"
     "audio"
     "video"
     "dialout"
     supplementary-groups))))

;; note these need to be updated when configurations are bootstrapped
;; run sudo cat /etc/wireguard/private.key | wg pubkey
(define wg-casper-peer
  (wireguard-peer
   (name "casper")
   (public-key "5rD5C1+++mx9oPNTod5qCT7COJ7lQ/E6zO2lkNSSzFY=")
   (allowed-ips '("192.168.99.0/24" ))))
(define wg-melchior-peer
  (wireguard-peer
   (name "melchior")
   (endpoint "omase.tk:51820")
   (public-key "16fC2yp/lZhns8Eqz5fGP4ccQ0aiBnl/7N9mIhmV11Y=")
   (allowed-ips '("192.168.99.0/24" "192.168.88.0/24"))
   (keep-alive 25)))
(define wg-router-peer
  )

(define melchior
  (magi-config
   (name "melchior")
   (os
    (operating-system
     (inherit magi)
     (host-name "melchior")
     (services (append
                (list
                 (service syncthing-service-type
			  (syncthing-configuration
			   (user "maya")
			   (arguments '("--no-browser" "--no-default-folder" "--log-max-size=1000"))))
                 (service ntp-service-type)
                 polkit-wheel-service
                 (service polkit-service-type)
                 (service elogind-service-type)
                 (dbus-service)
                 (service nginx-service-type
                          (nginx-configuration
                           (server-blocks
                            (list
                             http-redirect-to-https
                             transmission-nginx-configuration
                             jellyfin-nginx-configuration))))
                 (service certbot-service-type
                          (certbot-configuration
                           (email "maya.omase@disroot.org")
                           (certificates
                            (list
                             transmission-certbot-configuration))))
                 (service transmission-daemon-service-type
                          (transmission-daemon-configuration
                           (rpc-authentication-required? #t)
                           (rpc-username "melchior")
                           (rpc-password "{8a6cf535a13132c467ad438d7c71d8d127f2a62ae9J1MkgE")
                           (rpc-whitelist-enabled? #t)
                           (rpc-whitelist '("::1" "127.0.0.1" "192.168.88.*"))
                           (download-dir "/media/huge/downloads")
                           (incomplete-dir-enabled? #t)
                           (incomplete-dir "/media/huge/downloads/incomplete")))
                 (service docker-service-type)
                 (service openssh-service-type
                          (openssh-configuration
                           (password-authentication? #f)
                           (authorized-keys
                            `(("maya-casper" ,(local-file "configuration/ssh/maya-casper.pub"))))))
                 (service avahi-service-type)
                 (service wireguard-service-type
                          (wg-config '("192.168.99.1/32") wg-casper-peer))
                 (service static-networking-service-type
                          (list (static-networking
                                 (addresses
                                  (list (network-address
                                         (device "enp2s0")
                                         (value "192.168.88.2/24"))))
                                 (routes
                                  (list (network-route
                                         (destination "default")
                                         (gateway "192.168.88.1"))))
                                 (name-servers '("192.168.88.1" "8.8.8.8")))))
                 ;; (service nfs-service-type
                 ;;          (nfs-configuration
                 ;;           (exports `(("/media/huge/batocera"
                 ;;                       "*(rw,sync,all_squash,anonuid=42,anongid=42,no_subtree_check)")))))
                 jellyfin-service)
                magi-services
                %base-services))
     (users
      (list
       (user-account
        (name "batocera")
        (group "batocera")
        (uid 42)
        (system? #t)
        (home-directory "/media/huge/batocera")
        (shell (file-append shadow "/sbin/nologin")))))
     (groups (cons
              (user-group
               (name "batocera")
               (id 42)
               (system? #t))
              %base-groups))
     (file-systems (cons*
                    %magi-boot-file-system
                    %magi-root-file-system
                    (file-system
                     (device (file-system-label "huge"))
                     (mount-point "/media/huge")
                     (type "btrfs")
                     (options "compress-force=zstd"))
                    %base-file-systems))))
   (users (list
           (magi-user-config
            (name "maya")
            (account (maya-user-account)))))))

(define magi-bootloader
  (operating-system-bootloader magi-nonguix))
(define balthasar
  (magi-config
   (name "balthasar")
   (os
    (operating-system
     (inherit magi-nonguix)
     (host-name "balthasar")
     (keyboard-layout %cz-dvorak-ucw)
;     (kernel linux-lts)
;     (kernel-arguments %nvidia-kernel-arguments)
;     (kernel-loadable-modules %nvidia-kernel-loadable-modules)
     (bootloader
      (bootloader-configuration
       (inherit magi-bootloader)
       (default-entry 1)
       (menu-entries (list
                      (menu-entry
                       (label "Batocera")
                       (device "BATOCERA")
                       (linux "/boot/linux")
                       (linux-arguments '("label=BATOCERA" "console=tty3" "quiet" "loglevel=0" "vt.global_cursor_default=0"))
                       (initrd "/boot/initrd.gz"))))))
     (services
      (append
       (nonguix-desktop-services %desktop-services #:wayland? #t #:autologin "maya")
       (list
        (service gnome-desktop-service-type)
        (bluetooth-service #:auto-enable? #t)
        (service docker-service-type)
        (service openssh-service-type
                 (openssh-configuration
                  (authorized-keys
                   `(("maya-casper" ,(local-file "configuration/ssh/maya-casper.pub")))))))))
     (file-systems
      (cons*
       %magi-boot-file-system
       %magi-root-file-system
       %base-file-systems))))
   (users (list
           (magi-user-config
            (name "maya")
            (account (maya-user-account))
            (environment (home-environment
                          (packages `(,kodi/wayland
                                      ,@games
                                      ,@fonts
                                      ,@gnome-desktop
                                      ,@desktop))
                          (services
                           `(,home-syncthing-service
                             ,@emacs-services
                             ,@configuration-services
                             ,@dbus-services
                             ,@gnupg-services
                             ,@zsh-services
                             ,@pipewire-services
                             )))))))))

(define transform
  (options->transformation
   '((with-debug-info . "gnome-console"))))

(define casper
  (magi-config
   (name "casper")
   (os
    (operating-system
     (inherit magi-nonguix)
     (host-name "casper")
     (firmware (list linux-firmware))
     (keyboard-layout %cz-dvorak-ucw)
     (services (append (list
                        ;;jellyfin-service
		        (service gnome-desktop-service-type)
		        (set-xorg-configuration
		         (xorg-configuration
		          (keyboard-layout %cz-dvorak-ucw)))
                        (bluetooth-service #:auto-enable? #t)
                        (service docker-service-type
			         (docker-configuration))
		        ;; (service syncthing-service-type
			;;          (syncthing-configuration
			;;           (user "maya")
			;;           (arguments '("--no-browser" "--no-default-folder" "--log-max-size=1000")))))
                        (service spice-vdagent-service-type)
                        (service virtlog-service-type
                                 (virtlog-configuration))
                        (service libvirt-service-type
                                 (libvirt-configuration (unix-sock-group "libvirt")))
                        ;; (service wireguard-service-type
                        ;;          (wireguard-configuration
                        ;;           (addresses '("192.168.99.2/32"))
                        ;;           (port 51820)
                        ;;           (peers
                        ;;            (list
                        ;;             (wireguard-peer
                        ;;              (name "router")
                        ;;              (endpoint "omase.tk:51820")
                        ;;              (public-key "AS8jZ/t+r9CZQZEP2UHp2mv+cD14wiiaqaVUG4gXg2o=")
                        ;;              (allowed-ips '("0.0.0.0/0")))))))
                        )
                       magi-services
                       laptop-services
                       (nonguix-desktop-services %desktop-services #:wayland? #t)))
     (packages (append
	        (operating-system-packages magi-nonguix)
	        (map specification->package
		     (list
                      "wireguard-tools"
		      "syncthing"
		      "xf86-input-libinput"
                      "xf86-video-qxl"
                      "spice"
                      "ovmf"
                      "phodav"
		      "flatpak"
		      "fuse"
		      "podman"
		      "docker"
                      "runc"
		      "iptables"
		      "shadow"
		      "fprintd"
                      "opensc"))))
     (file-systems
      (cons*
       %magi-boot-file-system
       %magi-root-file-system
       %base-file-systems))))
   (users (list
           (magi-user-config
            (name "maya")
            (account
             (maya-user-account #:supplementary-groups '("docker" "libvirt" "kvm")))
            (environment
             (home-environment
              (packages `(
	                  ,@fonts
;;	                  ,@sway-desktop
;;                        ,@dwl-desktop
                          ,@gnome-desktop
	                  ,@desktop
                          ,@math
                          ,@virtualization
	                  ,@cc-toolchain
;;	                  ,@avr-toolchain
	                  ,@haskell-toolchain
                          ,@racket-toolchain
	                  ,@guile-toolchain
	                  ,@python-toolchain
                          ,@coq-toolchain
                          ,@apl-toolchain))
              (services
               `(,home-syncthing-service
                 ,@emacs-services
                 ,@sway-services
                 ,@configuration-services
                 ,@dbus-services
                 ,@gnupg-services
                 ,@zsh-services
                 ,@(mail-services "/home/maya/.mail" mail-accounts "Mája Tomášek" mailing-lists)
                 ,@pipewire-services
                 )))))))))

(dispatcher (list casper balthasar melchior))
