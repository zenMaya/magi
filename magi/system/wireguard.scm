(define-module (magi system wireguard)
  #:use-module (gnu services vpn))

(define-public (wg-config addresses . peers)
  (wireguard-configuration
   (addresses addresses)
   (port 51820)
   (peers peers)))
