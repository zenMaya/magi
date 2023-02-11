(define-module (shells gnome-toolchain)
  #:use-module (gnu packages)
  #:use-module (guix profiles))

(packages->manifest
 (map specification->package
      (list
       "gcc-toolchain"
       "gnome-builder"
       "gtk"
       "libinput"
       "dbus"
       "pkg-config"
       "ninja")))
