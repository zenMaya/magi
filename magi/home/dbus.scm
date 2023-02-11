(define-module (magi home dbus)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (gnu packages glib))

(define-public dbus-services
  (list
     ;; TODO: Make home-dbus-service-type
     (simple-service 'dbus-set-some-env-vars
		     home-environment-variables-service-type
		     '(("DBUS_SESSION_BUS_ADDRESS"
                        . "unix:path=$XDG_RUNTIME_DIR/bus")))
     (simple-service
      'dbus-add-shepherd-daemon
      home-shepherd-service-type
      (list
       (shepherd-service
        (provision '(dbus))
        (stop  #~(make-kill-destructor))
        (start #~(make-forkexec-constructor
                  (list #$(file-append dbus "/bin/dbus-daemon")
                        "--nofork"
                        "--session"
                        (string-append
                         "--address=" "unix:path="
                         (getenv "XDG_RUNTIME_DIR") "/bus"))
                  #:log-file (string-append
                                    (or (getenv "XDG_LOG_HOME")
                                        (format #f "~a/.local/var/log"
                                                (getenv "HOME")))
                                    "/dbus.log"))))))))
