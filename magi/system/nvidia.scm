(define-module (magi system nvidia)
  #:use-module (nongnu packages nvidia)
  #:use-module (gnu packages xorg)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services xorg)
  #:use-module (gnu services linux)
  #:use-module (guix transformations))

(define nvidia-mesa-transform
  (options->transformation
   '((with-graft . "mesa=nvda"))))

(define-public %nvidia-kernel-arguments
  (append '("modprobe.blacklist=nouveau" "nvidia_drm.modeset=1")
          %default-kernel-arguments))

(define-public %nvidia-kernel-loadable-modules
  (list nvidia-driver))

(define-public (nvidia-services xorg-base-configuration)
  (list
   (set-xorg-configuration
    (xorg-configuration
     (inherit xorg-base-configuration)
     (modules (cons nvidia-driver (xorg-configuration-modules xorg-base-configuration)))
     (server (nvidia-mesa-transform xorg-server))
     (drivers (cons
               "nvidia"
               (xorg-configuration-drivers xorg-base-configuration)))))
   (service kernel-module-loader-service-type
            '("ipmi_devintf"
              "nvidia"
              "nvidia_modeset"
              ;;              "nvidia_uvm"  ; allow proper sleeping, sadly it does turn off hw acc video encoding
              "nvidia_drm"))
   (simple-service 'custom-udev-rules udev-service-type (list nvidia-driver))))
