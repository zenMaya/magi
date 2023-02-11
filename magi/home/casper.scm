(define-module (magi home casper)
  #:use-module (magi home packages)
  #:use-module (magi home dbus)
  #:use-module (magi home pipewire)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home-services shellutils)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (guix gexp))

(define %mail-accounts
  (list
   (mail-account
    (id "maya.tomasek@disroot.org")
    (fqda "maya.tomasek@disroot.org")
    (type 'disroot))))
(define %mail-directory "~/.mail")

(home-environment
 (packages `(
	     ,@fonts
	     ,@sway-desktop
	     ,@desktop
	     ,@cc-toolchain
	     ,@avr-toolchain
	     ,@haskell-toolchain
             ,@racket-toolchain
	     ,@guile-toolchain
	     ,@python-toolchain
             ,@coq-toolchain
	     ,syncthing-package))
 (services
  `(
    ,@emacs-services
    ,@sway-services
    ,@configuration-services
    ;; ,syncthing-services
    ,@dbus-services
    ,@gnupg-services
    ,@zsh-services
    ;;,@pipewire-services
    ;; ,(service home-fish-service-type
    ;;           (home-fish-configuration
    ;;            (environment-variables '(
    ;;     				("PATH" . "$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH")))))
    ;;,(service home-zsh-autosuggestions-service-type)
    )))
