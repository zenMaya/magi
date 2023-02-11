;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Jesse Dowell <jessedowell@gmail.com>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (magi system docker)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system setuid)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages linux)               ;singularity
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix packages)

  #:export (docker-configuration
            docker-container
            docker-service-type
            singularity-service-type))

(define (pair-of-strings? val)
  (and (pair? val)
       (string? (car val))
       (string? (cdr val))))

(define (list-of-pair-of-strings? val)
  (list-of pair-of-strings?))

(define-configuration/no-serialization docker-container
  (name
   (symbol '())
   "Name of the docker container. Will be used to denote service to Shepherd and must be unique!
We recommend, that the name of the container is prefixed with @code{docker-}.")
  (documentation
   (string "")
   "Documentation on the docker container (optional). It will be used for the shepherd service.")
  (image-name
   (string #f)
   "A name of the image that will be used. (Note that the existence of the image
is not guaranteed by this daemon.)")
  (volumes
   (list-of-pair-of-strings '())
   "A list of volume bindings. In (HOST-PATH CONTAINER-PATH) format.")
  (ports
   (list-of-pair-of-strings '())
   "A list of port bindings. In (HOST-PORT CONTAINER-PORT) or (HOST-PORT CONTAINER-PORT OPTIONS) format.
For example, both port bindings are valid:

@lisp
(ports '((\"2222\" \"22\") (\"21\" \"21\" \"tcp\")))
@end lisp")
  (environments
   (list-of-pair-of-strings '())
   "A list of environment variables, inside the container environment, in (VARIABLE VALUE) format.")
  (network
   (string "none")
   "Network type.

Available types are:
@table @code
@c Copied from https://docs.docker.com/network/

@item none

The default option. For this container, disable all networking. Usually used in
conjunction with a custom network driver. none is not available for swarm services.

@item bridge

Bridge networks are usually used when your applications run in standalone
containers that need to communicate.

@item host

For standalone containers, remove network isolation between the container and the Docker host, 
and use the host’s networking directly.

@item overlay

Overlay networks connect multiple Docker daemons together and enable swarm services to
communicate with each other. You can also use overlay networks to facilitate
communication between a swarm service and a standalone container, or between
two standalone containers on different Docker daemons. This strategy removes
the need to do OS-level routing between these containers.

@item ipvlan

IPvlan networks give users total control over both IPv4 and IPv6 addressing.
The VLAN driver builds on top of that in giving operators complete control of
layer 2 VLAN tagging and even IPvlan L3 routing for users interested in underlay
network integration.

@item macvlan

Macvlan networks allow you to assign a MAC address to a container, making it appear
as a physical device on your network. The Docker daemon routes traffic to containers
by their MAC addresses. Using the macvlan driver is sometimes the best choice when
dealing with legacy applications that expect to be directly connected to the physical
network, rather than routed through the Docker host’s network stack.

@end table")
  (additional-arguments
   (list-of-strings '())
   "Additional arguments to the docker command line interface.")
  (container-command
   (list-of-strings '())
   "Command to send into the container.")
  (pid-file-timeout
   (number 5)
   "If the docker container does not show up in @code{docker ps} as @code{running} in less than pid-file-timeout seconds, the container is considered as failing to start.

Note that some containers take a really long time to start, so you should adjust it accordingly."))

(define (serialize-volumes config)
  "Serialize list of pairs into flat list of @code{(\"-v\" \"HOST_PATH:CONTAINER_PATH\" ...)}"
  (append-map
   (lambda (volume-bind)
     (list "-v" (apply format #f "~a:~a~^:~a" volume-bind)))
   (docker-container-volumes config)))

(define (serialize-ports config)
  "Serialize list of either pairs, or lists into flat list of
@code{(\"-p\" \"NUMBER:NUMBER\" \"-p\" \"NUMBER:NUMBER/PROTOCOL\" ...)}"
  (append-map
   (lambda (port-bind)
     (list "-p" (apply format #f "~a:~a~^/~a" port-bind)))
   (docker-container-ports config)))

(define (serialize-environments config)
  "Serialize list of pairs into flat list of @code{(\"-e\" \"VAR=val\" \"-e\" \"VAR=val\" ...)}."
  (append-map
   (lambda (env-bind)
     (list "-e" (apply format #f "~a=~a" env-bind)))
   (docker-container-environments config)))

(define (docker-container-startup-script docker-cli container-name cid-file config)
  "Return a program file, that executes the startup sequence of the @code{docker-container-shepherd-service}."
  (let* ((image-name (docker-container-image-name config))
         (volumes (serialize-volumes config))
         (ports (serialize-ports config))
         (envs (serialize-environments config))
         (network (docker-container-network config))
         (additional-arguments (docker-container-additional-arguments config))
         (container-command (docker-container-container-command config)))
    (with-imported-modules
     '((guix build utils))
     (program-file
      (string-append "start-" container-name "-container")
      #~(let ((docker (string-append #$docker-cli "/bin/docker")))
          (use-modules (guix build utils))
          ;; These two commands should fail
          ;; they are there as a failsafe to
          ;; prevent contamination from unremoved containers
          (system* docker "stop" #$container-name)
          (system* docker "rm" #$container-name)
          (apply invoke `(,docker
                           "run"
                           ,(string-append "--name=" #$container-name)
                           ;; Automatically remove the container when stopping
                           ;; If you want persistent data, you need to use
                           ;; volume binds or other methods.
                           "--rm"
                           ,(string-append "--network=" #$network)
                           ;; Write to a cid file the container id, this allows
                           ;; for shepherd to manage container even when the process
                           ;; itself gets detached from the container
                           "--cidfile" #$cid-file
                           #$@volumes
                           #$@ports
                           #$@envs
                           #$@additional-arguments
                           ,#$image-name
                           #$@container-command)))))))

(define (docker-container-shepherd-service docker-cli config)
  "Return a shepherd-service that runs CONTAINER."
  (let* ((container-name (symbol->string (docker-container-name config)))
         (cid-file (string-append "/var/run/docker/" container-name ".pid"))
         (pid-file-timeout (docker-container-pid-file-timeout config)))
    (shepherd-service
     (provision (list (docker-container-name config)))
     (requirement `(dockerd))
     (documentation (docker-container-documentation config))
     (start #~(apply make-forkexec-constructor
                     `(,(list #$(docker-container-startup-script docker-cli container-name cid-file config))
                       ;; Watch the cid-file instead of the docker run command, as the daemon can
                       ;; still be running even when the command terminates
                       #:pid-file-timeout #$pid-file-timeout)))
     (stop #~(lambda _
               (invoke
                (string-append #$docker-cli "/bin/docker")
                "stop"
                #$container-name)
               ;; Shepherd expects the stop command to return #f if it succeeds
               ;; docker stop should always succeed
               #f)))))

(define (list-of-docker-containers? val)
  (list-of docker-container?))

(define-configuration docker-configuration
  (docker
   (file-like docker)
   "Docker daemon package.")
  (docker-cli
   (file-like docker-cli)
   "Docker client package.")
  (containerd
   (file-like containerd)
   "containerd package.")
  (proxy
   (file-like docker-libnetwork-cmd-proxy)
   "The proxy package to support inter-container and outside-container
loop-back communications.")
  (enable-proxy?
   (boolean #t)
   "Enable or disable the user-land proxy (enabled by default).")
  (debug?
   (boolean #f)
   "Enable or disable debug output.")
  (enable-iptables?
   (boolean #t)
   "Enable addition of iptables rules (enabled by default).")
  (environment-variables
   (list '())
   "Environment variables to set for dockerd")
  (containers
   (list-of-docker-containers '())
   "List of docker containers to run as shepherd services.")
  (no-serialization))

(define (docker-container-shepherd-services config)
  "Return shepherd services for all containers inside config."
  (let ((docker-cli (docker-configuration-docker-cli config)))
    (map
     (lambda (container)
       (docker-container-shepherd-service
        docker-cli
        container))
     (docker-configuration-containers config))))

(define %docker-accounts
  (list (user-group (name "docker") (system? #t))))

(define (%containerd-activation config)
  (let ((state-dir "/var/lib/containerd"))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$state-dir))))

(define (%docker-activation config)
  (%containerd-activation config)
  (let ((state-dir "/var/lib/docker"))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$state-dir))))

(define (containerd-shepherd-service config)
  (let* ((package (docker-configuration-containerd config))
         (debug? (docker-configuration-debug? config))
         (containerd (docker-configuration-containerd config)))
    (shepherd-service
     (documentation "containerd daemon.")
     (provision '(containerd))
     (start #~(make-forkexec-constructor
               (list (string-append #$package "/bin/containerd")
                     #$@(if debug?
                            '("--log-level=debug")
                            '()))
               ;; For finding containerd-shim binary.
               #:environment-variables
               (list (string-append "PATH=" #$containerd "/bin"))
               #:pid-file "/run/containerd/containerd.pid"
               #:pid-file-timeout 300
               #:log-file "/var/log/containerd.log"))
     (stop #~(make-kill-destructor)))))

(define (docker-shepherd-service config)
  (let* ((docker (docker-configuration-docker config))
         (enable-proxy? (docker-configuration-enable-proxy? config))
         (enable-iptables? (docker-configuration-enable-iptables? config))
         (environment-variables (docker-configuration-environment-variables config))
         (proxy (docker-configuration-proxy config))
         (debug? (docker-configuration-debug? config)))
    (shepherd-service
           (documentation "Docker daemon.")
           (provision '(dockerd))
           (requirement '(containerd
                          dbus-system
                          elogind
                          file-system-/sys/fs/cgroup/blkio
                          file-system-/sys/fs/cgroup/cpu
                          file-system-/sys/fs/cgroup/cpuset
                          file-system-/sys/fs/cgroup/devices
                          file-system-/sys/fs/cgroup/memory
                          file-system-/sys/fs/cgroup/pids
                          networking
                          udev))
           (start #~(make-forkexec-constructor
                     (list (string-append #$docker "/bin/dockerd")
                           "-p" "/var/run/docker.pid"
                           #$@(if debug?
                                  '("--debug" "--log-level=debug")
                                  '())
                           #$@(if enable-proxy?
                                  (list "--userland-proxy=true"
                                        #~(string-append
                                           "--userland-proxy-path=" #$proxy "/bin/proxy"))
                                  '("--userland-proxy=false"))
                           (if #$enable-iptables?
                               "--iptables"
                               "--iptables=false")
                           "--containerd" "/run/containerd/containerd.sock")
                     #:environment-variables
                     (list #$@environment-variables)
                     #:pid-file "/var/run/docker.pid"
                     #:log-file "/var/log/docker.log"))
           (stop #~(make-kill-destructor)))))

(define docker-service-type
  (service-type (name 'docker)
                (description "Provide capability to run Docker application
bundles in Docker containers and optionally wrap those containers in shepherd services.")
                (extensions
                 (list
                  ;; Make sure the 'docker' command is available.
                  (service-extension profile-service-type
                                     (compose list docker-configuration-docker-cli))
                  (service-extension activation-service-type
                                     %docker-activation)
                  (service-extension shepherd-root-service-type
                                     (lambda (config)
                                       (cons* (containerd-shepherd-service config)
                                              (docker-shepherd-service config)
                                              (docker-container-shepherd-services config))))
                  (service-extension account-service-type
                                     (const %docker-accounts))))
                (compose concatenate)
                (extend (lambda (config containers)
                          (docker-configuration
                           (inherit config)
                           (containers (append containers (docker-configuration-containers config))))))
                (default-value (docker-configuration))))


;;;
;;; Singularity.
;;;

(define %singularity-activation
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (define %mount-directory
          "/var/singularity/mnt/")

        ;; Create the directories that Singularity 2.6 expects to find.  Make
        ;; them #o755 like the 'install-data-hook' rule in 'Makefile.am' of
        ;; Singularity 2.6.1.
        (for-each (lambda (directory)
                    (let ((directory (string-append %mount-directory
                                                    directory)))
                      (mkdir-p directory)
                      (chmod directory #o755)))
                  '("container" "final" "overlay" "session"))
        (chmod %mount-directory #o755))))

(define (singularity-setuid-programs singularity)
  "Return the setuid-root programs that SINGULARITY needs."
  (define helpers
    ;; The helpers, under a meaningful name.
    (computed-file "singularity-setuid-helpers"
                   #~(begin
                       (mkdir #$output)
                       (for-each (lambda (program)
                                   (symlink (string-append #$singularity
                                                           "/libexec/singularity"
                                                           "/bin/"
                                                           program "-suid")
                                            (string-append #$output
                                                           "/singularity-"
                                                           program
                                                           "-helper")))
                                 '("action" "mount" "start")))))

  (map file-like->setuid-program
       (list (file-append helpers "/singularity-action-helper")
             (file-append helpers "/singularity-mount-helper")
             (file-append helpers "/singularity-start-helper"))))

(define singularity-service-type
  (service-type (name 'singularity)
                (description
                 "Install the Singularity application bundle tool.")
                (extensions
                 (list (service-extension setuid-program-service-type
                                          singularity-setuid-programs)
                       (service-extension activation-service-type
                                          (const %singularity-activation))))
                (default-value singularity)))
