(define-module (magi home packages)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services syncthing)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services shells)
  #:use-module (rde home services wm)
  #:use-module (gnu home-services gnupg)
  #:use-module (rde home services shellutils)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages base)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages avahi)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages)
  #:use-module (gnu packages php)
  #:use-module (gnu packages emulators)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages text-editors)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (rde packages emacs)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages syncthing)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages spice)
;;; for custom packages
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix build python-build-system) #:prefix python:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public decaf-emu
  (package
    (name "decaf-emu")
    (version "e8c9af3057a7d94f6e970406eb1ba1c37c87b4d1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/decaf-emu/decaf-emu.git")
                    (commit version)
                    (recursive? #t)))
              (sha256
               (base32 "08r7si93a27b9xkvbp7jwfs77rzz3g956sijqgnvqqmm3mj8mcn7"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f #:configure-flags '("-DDECAF_VULKAN=OFF")))
    (inputs (list
             c-ares
             curl
             ffmpeg
             libuv
             openssl
             ;;vulkan-headers
             ;;vulkan-loader
             sdl2
             zlib
             qtbase
             qtsvg
             qtx11extras
             ;;xkbcommon
             mesa
             glu
             python
             glslang))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public nlohmann-json
  (package
    (name "nlohmann-json")
    (version "3.11.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/nlohmann/json/releases/download/v" version "/json.tar.xz"))
              (sha256
               (base32 "07rlilknhpk7rfd3j91x8k1va2pci3iwaark7zhm48j29fzjcjwc"))))
    (build-system cmake-build-system)
    (arguments ;; '(#:install-plan
     ;;   '(("include/" "include/")))
     ;; These fail spectacularly
     '(#:tests? #f
       #:configure-flags '("-DJSON_BuildTests=OFF")))
    (home-page "https://github.com/nlohmann/json")
    (synopsis "")
    (description "")
    (license license:expat)))

;; (define-public sdl2.24
;;   (package
;;    (inherit sdl2)
;;    (version "2.24.0")
;;    (source (origin
;;             (inherit (package-source sdl2))
;;             (sha256
;;              (base32
;;               "15vd9najhjh6s9z9hhx7zp51iby690a1g3h7kcwjvyb82x5w7r4i"))))
;;    (arguments
;;      (substitute-keyword-arguments (package-arguments sdl)
;;        ((#:configure-flags flags)
;;         `(append '("--disable-wayland-shared" 
;;                    "--enable-video-wayland"
;;                    "--disable-libdecor-shared"
;;                    "--enable-video-kmsdrm" "--disable-kmsdrm-shared"
;;                    "--enable-video-x11" "--disable-x11-shared"
;;                    "--enable-video-vulkan"
;;                    "--disable-esd-shared"
;;                    "--enable-joystick"
;;                    "--enable-hidapi" "--enable-audio"
;;                    "--enable-pulseaudio" "--disable-pulseaudio-shared"
;;                    "--enable-pipewire" "--disable-pipewire-shared"
;;                    "--enable-hidapi-joystick" "--enable-hidapi-libusb"
;;                    )
;;                  ,flags))
;;        ((#:make-flags flags ''())
;;         `(cons*
;;           ;; SDL dlopens libudev, so make sure it is in rpath. This overrides
;;           ;; the LDFLAG set in sdl’s configure-flags, which isn’t necessary
;;           ;; as sdl2 includes Mesa by default.
;;           (string-append "LDFLAGS=-Wl,-rpath,"
;;                          (assoc-ref %build-inputs "eudev") "/lib")
;;           ,flags))))
;;    (propagated-inputs (append
;;                        `(("vulkan-loader" ,vulkan-loader))
;;                        (package-propagated-inputs sdl2)))
;;    (native-inputs (append
;;                    `(("python" ,python)
;;                      ("perl" ,perl))
;;                    (package-native-inputs sdl2)))
;;    (inputs (append
;;             `(("pipewire" ,pipewire-0.3)
;;               ("libxi" ,libxi)
;;               ("hidapi" ,hidapi)
;;               ("libusb" ,libusb)
;;               ("vulkan-headers" ,vulkan-headers)
;;               ("vulkan-loader" ,vulkan-loader))
;;             (package-inputs sdl2)))))

(define-public yuzu
  (package
    (name "yuzu")
    (version "1185")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://github.com/yuzu-emu/yuzu-mainline")
                (commit (string-append "mainline-0-" version))
                (recursive? #t)))
              (sha256
               (base32 "0bs3q7qyv163d5axw1yzjpimpqbh4w2ipqhzml55ivxlgdb8dj8q"))))
    ;; Update c-toolchain, as yuzu depends on features from gcc-11
    ;; And qtbase-5 headers won't build on gcc-11 (see https://bugreports.qt.io/browse/QTBUG-91909)
    ;; so we need to provide a custom qtbase-5 with a patch
    ;; solved with a patch submitted upstream
    (build-system (build-system-with-c-toolchain cmake-build-system
                                                 `(("toolchain" ,gcc-toolchain-12))))
    (arguments '(#:configure-flags
                 '("-DENABLE_WEB_SERVICE=OFF" ;; disable telemetry
                   ;; disable checking for submodules, this check fails
                   ;; even though, the submodules are included
                   "-DYUZU_CHECK_SUBMODULES=OFF"
                   ;; Disable bundled software, everything should be
                   ;; built from source or in the store already.
                   "-DYUZU_USE_BUNDLED_VCPKG=OFF"
                   "-DYUZU_USE_BUNDLED_OPUS=OFF"
                   "-DYUZU_USE_BUNDLED_FFMPEG=OFF"
                   "-DYUZU_USE_BUNDLED_LIBUSB=OFF"
                   ;; These options force to use our own SDL
                   ;; Which has statically linked libraries
                   ;; contrary to the default dynamic linking.
                   ;; Don't use SDL from submodules
                   "-DYUZU_USE_EXTERNAL_SDL2=OFF"
                   ;; Don't use bundled SDL
                   "-DYUZU_USE_BUNDLED_SDL2=OFF")))
    (native-inputs
     (list
      pkg-config
      git))
    (inputs (list
             ffmpeg
             alsa-lib
             pipewire-0.3
             pulseaudio
             libsamplerate
             hidapi
             sdl2
             opus
             qtbase-5/fix-template
             qtmultimedia-5
             wayland
             egl-wayland
             libxkbcommon
             vulkan-headers
             vulkan-loader
             boost
             catch2
             fmt
             lz4
             nlohmann-json
             openssl
             libusb
             zlib
             `(,zstd "lib")
             libva
             python
             perl
             eudev
             glslang))
    (home-page "https://yuzu-emu.org")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public emulationstation-desktop-edition
  (package
    (name "emulationstation-desktop-edition")
    (version "1.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://gitlab.com/es-de/emulationstation-de/-/archive/v" version "/emulationstation-de-v" version ".tar.gz"))
              (sha256
               (base32
                "172hfvgqkgiqizk4li2v7rxjnq19gqdzc1qy9wfzy5npjrw2dkd4"))))
    (build-system cmake-build-system)
    (native-inputs (list
                    pkg-config))
    (inputs (list
             sdl2
             ffmpeg
             freeimage
             freetype
             curl
             pugixml
             alsa-lib
             mesa
             mesa-headers
             vlc))
    (arguments '(#:tests? #f))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

;; (define-public emacs-fontaine
;;   (package
;;   (name "emacs-fontaine")
;;   (version "0.4.1")
;;   (source (origin
;;             (method url-fetch)
;;             (uri (string-append "https://elpa.gnu.org/packages/fontaine-"
;;                                 version ".tar"))
;;             (sha256
;;              (base32
;;               "0szj9ys7bkj6cwg2bmb7sniyzjzdy3f7qm9r90grrgs5iir3k2qa"))))
;;   (build-system emacs-build-system)
;;   (home-page "https://git.sr.ht/~protesilaos/fontaine")
;;   (synopsis "Set font configurations using presets")
;;   (description "")
;;   (license license:gpl3+)))

(define-public emacs-parinfer-rust-mode
  (package
   (name "emacs-parinfer-rust-mode")
   (version "20230204.1915")
   (source (origin
             (method git-fetch)
             (uri (git-reference
                    (url
                     "https://github.com/justinbarclay/parinfer-rust-mode.git")
                    (commit "332c7f47426f0519dc5c24dda82afdb1aa8b61ee")))
             (sha256
              (base32
               "12rx37js82lxq80bq2rpzgw8miw7ni0hx9xhxa775c67idyppjzv"))))
   (build-system emacs-build-system)
   (home-page "https://github.com/justinbarclay/parinfer-rust-mode")
   (synopsis "An interface for the parinfer-rust library")
   (description "")
   (license #f)))

(define-public emacs-embark-consult
  (package
    (name "emacs-embark-consult")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://elpa.gnu.org/packages/embark-consult-" version
             ".tar"))
       (sha256
        (base32
         "1c8rx9ikazbnap293ab6s26djikdy85i7z330wdwwrgmipkfawaj"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-embark emacs-consult))
    (home-page "https://github.com/oantolin/embark")
    (synopsis "Consult integration for Embark")
    (description "")
    (license license:gpl3+)))


(define-public emacs-nano-theme
  (package
    (name "emacs-nano-theme")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://elpa.gnu.org/packages/nano-theme-"
             version
             ".tar"))
       (sha256
        (base32 "1nq5x46467vnsfg3fzb0qyg97xpnwsvbqg8frdjil5zq5fhsgmrz"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/rougier/nano-theme")
    (synopsis "N Λ N O theme")
    (description
     "N Λ N O theme is a consistent theme that comes in two flavors: 
so@itemize
@item a light theme that is based on Material (@url{https://material.io/})
@item a dark theme that is based on Nord (@url{https://www.nordtheme.com/})
@end itemize

A theme is fully defined by a set of (1+6) faces as explained in this paper
@url{https://arxiv.org/abs/2008.06030}

Recommended font is \"Roboto Mono\" (package @code{fonts-google-roboto}) or \"Roboto Mono Nerd\" if you want to benefit
from all the fancy glyphs.")
    (license license:gpl3+)))

(define-public emacs-nano-agenda
  (package
    (name "emacs-nano-agenda")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://elpa.gnu.org/packages/nano-agenda-"
             version
             ".tar"))
       (sha256
        (base32 "0j29fwc273mjdlj83h1a46sb7z3j066qqnp2i78kn2pmgjg27szb"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/rougier/nano-agenda")
    (synopsis "N Λ N O agenda")
    (description
     "N Λ N O agenda is a minimal view of your org agenda files.  It displays a
calendar view of current month (or the month corresponding to the current
selected date) alongside a view of your agenda displaying timestamped entries.
The agenda can be navigated using arrows keys and killed using \"q\", \"return\" or
\"escape\".")
    (license license:gpl3+)))

(define emacs-csharp-mode
  (package
    (name "emacs-csharp-mode")
    (version "20211124.1105")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacs-csharp/csharp-mode.git")
             (commit "fa06dfa206812476217ada6c4178de34ff1efc42")))
       (sha256
        (base32 "1d8capbzdr69sr4xhz2l8aczys1ry5ns1k873575wp8xfdp02ppq"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/emacs-csharp/csharp-mode")
    (synopsis "C# mode derived mode")
    (description "No description available.")
    (license #f)))

(define-public emacs-flymake-swi-prolog
  (package
    (name "emacs-flymake-swi-prolog")
    (version "20220404.950")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~eshel/flymake-swi-prolog")
             (commit "ae0e4b706a40b71c007ed6cb0ec5425d49bea4c3")))
       (sha256
        (base32 "07wpm394vm026ihw1lzndmrw8bx9fk48bh6569llvcw439ni0mzy"))))
    (build-system emacs-build-system)
    (home-page "https://git.sr.ht/~eshel/flymake-swi-prolog")
    (synopsis "A Flymake backend for SWI-Prolog")
    (description
     "This package provides a Flymake backend for SWI-Prolog source code.")
    (license #f)))

(define-public emacs-haskell-mode
  (package
    (name "emacs-haskell-mode")
    (version "17.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/haskell/haskell-mode")
             (commit "5a9f8072c7b9168f0a8409adf9d62a3e4ad4ea3d")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0np1wrwdq7b9hpqpl9liampacnkx6diphyk8h2sbz2mfn9qr7pxs"))))
    (propagated-inputs
     (list emacs-dash))
    (native-inputs
     (list emacs-minimal emacs-el-search emacs-stream texinfo git))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~(list
                      (string-append "EMACS=" #$emacs-minimal "/bin/emacs"))
      #:modules `((ice-9 match)
                  (srfi srfi-26)
                  ((guix build emacs-build-system) #:prefix emacs:)
                  ,@%gnu-build-system-modules)
      #:imported-modules `(,@%gnu-build-system-modules
                           (guix build emacs-build-system)
                           (guix build emacs-utils))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-before 'build 'pre-build
            (lambda* (#:key inputs #:allow-other-keys)
              (define (el-dir store-dir)
                (match (find-files store-dir "\\.el$")
                  ((f1 f2 ...) (dirname f1))
                  (_ "")))

              (let ((sh (search-input-file inputs "/bin/sh")))
                (define emacs-prefix? (cut string-prefix? "emacs-" <>))

                (setenv "SHELL" "sh")
                (setenv "EMACSLOADPATH"
                        (string-concatenate
                         (map (match-lambda
                                (((? emacs-prefix? name) . dir)
                                 (string-append (el-dir dir) ":"))
                                (_ ""))
                              inputs)))
                (substitute* (find-files "." "\\.el") (("/bin/sh") sh)))))
          (add-before 'check 'delete-failing-tests
            ;; XXX: these tests require GHC executable, which would be a big
            ;; native input.
            (lambda _
              (with-directory-excursion "tests"
                ;; File `haskell-indent-tests.el' fails with
                ;; `haskell-indent-put-region-in-literate-2'
                ;; on Emacs 27.1+
                ;; XXX: https://github.com/haskell/haskell-mode/issues/1714
                (for-each delete-file
                          '("haskell-indent-tests.el"
                            "haskell-customize-tests.el"
                            "inferior-haskell-tests.el")))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (el-dir (emacs:elpa-directory out))
                     (doc (string-append
                           out "/share/doc/haskell-mode-" #$version))
                     (info (string-append out "/share/info")))
                (define (copy-to-dir dir files)
                  (for-each (lambda (f)
                              (install-file f dir))
                            files))

                (with-directory-excursion "doc"
                  (invoke "makeinfo" "haskell-mode.texi")
                  (install-file "haskell-mode.info" info))
                (copy-to-dir doc '("CONTRIBUTING.md" "NEWS" "README.md"))
                (copy-to-dir el-dir (find-files "." "\\.elc?"))))))))
    (home-page "https://github.com/haskell/haskell-mode")
    (synopsis "Haskell mode for Emacs")
    (description
     "This is an Emacs mode for editing, debugging and developing Haskell
programs.")
    (license license:gpl3+)))

(define-public emacs-eldoc-box
  (package
    (name "emacs-eldoc-box")
    (version "20220506.28")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/casouri/eldoc-box.git")
                    (commit "8d523f4fddbd8986340cf76f349ab18c0b3d5581")))
              (sha256
               (base32
                "0sa97bxkl04s3kg1fd6c6dynp9jklw74jkgny2qhk1d2lzc64bja"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/casouri/eldoc-box")
    (synopsis "Display documentation in childframe")
    (description " See documentation in README.org or visit homepage")
    (license license:gpl3+)))

(define-public emacs-zig-mode
  (package
    (name "emacs-zig-mode")
    (version "20220521.1148")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ziglang/zig-mode.git")
                    (commit "dbc648f5bca8f3b9ca2cc7827f326f5530115144")))
              (sha256
               (base32
                "0hwkkwhc5b2pzyqa2h0xw8wxijsrp1fk70fhyv8hx19shzlc4la3"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/zig-lang/zig-mode")
    (synopsis "A major mode for the Zig programming language")
    (description
     "This package provides a major mode for the Zig programming languages.

See documentation on https://github.com/zig-lang/zig-mode")
    (license license:gpl3+)))

(define-public emacs-spotify
  (package
    (name "emacs-spotify")
    (version "6c5d3892804a311284ede0dcdda7fffa6e733b62")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/SnootierMoon/emacs-spotify.git")
                    (commit version)))
              (sha256
               (base32
                "092km4wnrih33c8l8x4niyg3m1vi5ifd2v6v4rrbf0fdzz6vl043"))))
    (propagated-inputs (list emacs-simple-httpd))
    (build-system emacs-build-system)
    (home-page "https://github.com/SnootierMoon/emacs-spotify.git")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public emacs-svelte-mode
  (package
    (name "emacs-svelte-mode")
    (version "20211016.652")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/leafOfTree/svelte-mode.git")
                    (commit "6a1d4274af7f4c0f271f77bd96678c3dd1fa68e1")))
              (sha256
               (base32
                "058mxzcrxqzsax21bs50vysr4ia3jcig83xbns0vhziqpj220yl1"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/leafOfTree/svelte-mode")
    (synopsis "Emacs major mode for Svelte")
    (description
     "This major mode includes JavaScript/CSS and other language modes as submode in
html-mode.  Mainly based on mhtml-mode.")
    (license license:lgpl3+)))

(define-public emacs-maxima
  (package
    (name "emacs-maxima")
    (version "20220531.1751")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/sasanidas/maxima.git")
                    (commit "1913ee496bb09430e85f76dfadf8ba4d4f95420f")))
              (sha256
               (base32
                "1milqql0p9gp4dn9phn4fw1izf37wizpirvmzh5s71rwzrr6a9ix"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-s emacs-test-simple))
    (arguments
     '(#:include '("^maxima.el$" "^maxima-font-lock.el$")
       #:exclude '()))
    (home-page "https://gitlab.com/sasanidas/maxima")
    (synopsis "Major mode for Maxima")
    (description
     "Quick intro To install, put this file (as well as maxima-font-lock.el) somewhere
in your Emacs load path.  To make sure that `maxima.el is loaded when necessary,
whether to edit a file in maxima mode or interact with Maxima in an Emacs
buffer, put the lines (or for use-package users see below) (autoload maxima-mode
\"maxima\" \"Maxima mode\" t) (autoload maxima \"maxima\" \"Maxima interaction\" t) in
your `.emacs file.  If you want any file ending in `.mac to begin in
`maxima-mode', for example, put the line (setq auto-mode-alist (cons (\"\\\\.mac\" .
 maxima-mode) auto-mode-alist)) to your `.emacs file.  for users of use-package,
the maxima package can be loaded with the form (use-package maxima :init
(add-hook maxima-mode-hook #'maxima-hook-function) (add-hook
maxima-inferior-mode-hook #'maxima-hook-function) (setq         
org-format-latex-options (plist-put org-format-latex-options :scale 2.0)        
maxima-display-maxima-buffer nil) :mode (\"\\\\.mac\\\\'\" .  maxima-mode)
:interpreter (\"maxima\" .  maxima-mode))")
    (license #f)))

(define-public emacs-gnu-apl-mode
  (package
    (name "emacs-gnu-apl-mode")
    (version "20220404.341")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lokedhs/gnu-apl-mode.git")
                    (commit "c8695b0d55b5167263a843252ffd21a589018427")))
              (sha256
               (base32
                "03hwnzzxn5d1wdw93dgznflsx9m9hb133gv54pbrij2454pkvm4g"))))
    (build-system emacs-build-system)
    (home-page "http://www.gnu.org/software/apl/")
    (synopsis "Integrate GNU APL with Emacs")
    (description
     "Emacs mode for GNU APL This mode provides both normal editing facilities for APL
code as well as an interactive mode.  The interactive mode is started using the
command ‘gnu-apl’.  The mode provides two different ways to input APL symbols.
The first method is enabled by default, and simply binds keys with the \"super\"
modifier.  The problem with this method is that the \"super\" modifier has to be
enabled, and any shortcuts added by the operating system that uses this key has
to be changed.  The other method is a bit more cumbersome to use, but it's
pretty much guaranteed to work everywhere.  Simply enable the input mode using
C-\\ (‘toggle-input-method’) and choose APL-Z. Once this mode is enabled, press
\".\" (period) followed by a letter to generate the corresponding symbol.")
    (license #f)))

(define-public emacs-magit-section
  (package
    (name "emacs-magit-section")
    (version "20230106.1659")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/magit/magit.git")
                    (commit "b908c79b44f5c282eec44f19fc1d9967f041dd5c")))
              (sha256
               (base32
                "1wybli5xrxkn8b7d7nm7h44avip9pjc24ig73hh15xg2wjm90zzc"))))
    (build-system emacs-build-system)
    (native-inputs
     (list texinfo))
    (propagated-inputs (list emacs-compat emacs-dash))
    (arguments
     '(#:include '("^magit-section.el$" "^magit-section-pkg.el$"
                   "^docs/magit-section.texi$"
                   "^Documentation/magit-section.texi$")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'build-info-manual
           (lambda _
             (invoke "make" "info")
             ;; Copy info files to the lisp directory, which acts as
             ;; the root of the project for the emacs-build-system.
             (for-each (lambda (f)
                         (install-file f "lisp"))
                       (find-files "Documentation" "\\.info$"))))
         (replace 'expand-load-path
           (lambda args
             (with-directory-excursion "lisp"
               (apply (assoc-ref %standard-phases 'expand-load-path) args))))
         (replace 'install
           (lambda args
             (with-directory-excursion "lisp"
               (apply (assoc-ref %standard-phases 'install) args)))))))
    (home-page "https://github.com/magit/magit")
    (synopsis "Sections for read-only buffers.")
    (description
     "This package implements the main user interface of Magit — the collapsible
sections that make up its buffers.  This package used to be distributed as part
of Magit but now it can also be used by other packages that have nothing to do
with Magit or Git.")
    (license #f)))

(define-public emacs-bufler
  (package
    (name "emacs-bufler")
    (version "20221031.1852")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/alphapapa/bufler.el.git")
                    (commit "bf5fdccbae6bb6dc51e31dc282805e32bb41e412")))
              (sha256
               (base32
                "142ql507mb7w6l3mr1y4914znnikab5vh8sm2q35pfvka383k1r7"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-dash emacs-f emacs-pretty-hydra
                             emacs-magit-section emacs-map))
    (arguments
     '(#:include '("^[^/]+.el$" "^[^/]+.el.in$"
                   "^dir$"
                   "^[^/]+.info$"
                   "^[^/]+.texi$"
                   "^[^/]+.texinfo$"
                   "^doc/dir$"
                   "^doc/[^/]+.info$"
                   "^doc/[^/]+.texi$"
                   "^doc/[^/]+.texinfo$")
       #:exclude '("^.dir-locals.el$" "^test.el$" "^tests.el$" "^[^/]+-test.el$"
                   "^[^/]+-tests.el$" "^helm-bufler.el$")))
    (home-page "https://github.com/alphapapa/bufler.el")
    (synopsis "Group buffers into workspaces with programmable rules")
    (description
     "Bufler is like a butler for your buffers, presenting them to you in an organized
way based on your instructions.  The instructions are written as grouping rules
in a simple language, allowing you to customize the way buffers are grouped.
The default rules are designed to be generally useful, so you don't have to
write your own.  It also provides a workspace mode which allows frames to focus
on buffers in certain groups.  Since the groups are created automatically, the
workspaces are created dynamically, rather than requiring you to put buffers in
workspaces manually.")
    (license #f)))


(define dwl-configured
  (package
    (inherit dwl)
    (version "ba7dcb2dea2d5dc46b73b6b9194daab94a325a05")
    (inputs (list (specification->package "wlroots@0.15.1")))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/djpohly/dwl")
             (commit version)))
       (file-name (string-append (package-name dwl) "-" version))
       (sha256
        (base32 "17ky2f278dipy9qwapjrg5fkw23qkkpf12rlp0899i41g20r7fj8"))     
       ;; (patches (list
       ;;           (origin
       ;;            (uri "https://git.sr.ht/~raphi/dwl/blob/master/patches/wayland-ipc.patch")
       ;;            (method url-fetch)
       ;;            (file-name "ipc.patch")
       ;;            (sha256
       ;;             (base32 "1q9910ckhnyz2yhmgj38kg1bsyv4nffkb30vr7fs75mfqw013mdhb")))))
       (snippet #~(symlink #$(local-file "../../configuration/dwl/config.h") "config.h"))))))
       
(define-public nyxt-extra
  (package
    (inherit nyxt)
    (inputs (cons
             `("slynk" ,sbcl-slynk)
             (package-inputs nyxt)))))

(define-public fonts
  (map specification->package
       (list
        "font-fira-code"
        "font-google-roboto"
        "font-awesome"
        "font-iosevka"
        "font-iosevka-aile"
        "font-iosevka-term"
        "font-tex-gyre")))

(define-public sway-desktop
  (map specification->package
       (list
        "alacritty"
        "bemenu"
        "j4-dmenu-desktop"
        "playerctl"
        "waybar"
        "wob"
        "brightnessctl"
        "xdg-utils"
        "jq"
        "anki")))

(define-public dwl-desktop
  (list
   dwl-configured))

(define-public gnome-desktop
  (map specification->package
       (list
        "gnome-shell-extension-gsconnect"
        "gnome-shell-extension-sound-output-device-chooser"
        "gnome-tweaks")))

(define-public utilities
  (map specification->package+output
       (list
        "waypipe"
        "ripgrep"
        "curl"
        "slirp4netns"
        "nmap"
        "gnupg"
        "password-store"
        "notmuch"
        "ifuse" ;iphone as a fuse filesystem
        "usbmuxd"
        "libimobiledevice")))

(define-public math
  (map specification->package+output
       (list
        "maxima"
        "wxmaxima"
        "texlive"
        "python-pygments")))

(define-public browsers
  (append 
   (map specification->package
        (list
         ;;      "icecat"
         "firefox"))
   (list nyxt-extra)))

(define-public desktop
  (append
   utilities
   browsers
   (map specification->package
        (list
         "flatpak"
         "vlc"
         ;;         "steam-devices-udev-rules"
         "calibre"
         "darktable"
         "mpv"
;;;         "gaupol"
         "font-openmoji"
         "transmission-remote-gtk"))))

(define-public games
  (list
   emulationstation-desktop-edition
   retroarch
   decaf-emu
   yuzu))

(define-public music
  (map specification->package
       (list
        "musescore"
        "ardour"
        "zam-plugins"
        "lv2-mda-piano"
        "dpf-plugins"
        "caps-plugins-lv2"
        "avldrums-lv2"
        "hydrogen"
        "helm"
        "qsynth"
        "amsynth")))

(define-public virtualization
  (list
   qemu
;;   spice
   ovmf
   virt-manager))

(define-public cc-toolchain
  (map specification->package
       (list
        "gcc-toolchain"
        "man-pages"
        "meson"
        "ninja"
        "make"
        "gdb"
        "rr"
        "pkg-config"
        "cmake"
        "automake"
        "autoconf"
        "libtool"
        "ccls"
        ;;"clang-toolchain"
        "bear"
        "doxygen"
        "gmp"
        "mpfr")))

(define-public jvm-toolchain
  (map specification->package
       (list
        "openjdk")))

(define-public avr-toolchain
  (map specification->package
       (list
        "avr-toolchain")))

(define-public haskell-toolchain
  (map specification->package
       (list
                                        ;       "gmp"
                                        ;       "ncurses"
                                        ;       "xz"
        "cabal-install"
        "ghc"
        "hlint"
        "hoogle"
        "stylish-haskell"
        "readline")))
        

(define-public guile-toolchain
  (map specification->package
       (list
        "guile"
        "guile-colorized"
        "guile-readline"
        "guile-syntax-highlight"
        "nyacc")))

(define-public racket-toolchain
  (map specification->package
       (list
        "racket")))

(define-public python-toolchain
  (map specification->package
       (list
        "python"
        "python-lsp-server")))

(define-public coq-toolchain
  (map specification->package
       (list
        "coq")))

(define-public apl-toolchain
  (map specification->package
       (list
        "apl")))

(define home-syncthing-shepherd-service
  (shepherd-service
   (provision '(syncthing))
   (documentation "Run and control syncthing.")
   (start #~(make-forkexec-constructor
             (list #$(file-append syncthing "/bin/syncthing")
                   "--no-browser" "--no-default-folder" "--log-max-size=1000")))
   (stop #~(make-kill-destructor))))

(define-public home-syncthing-service
  (simple-service 'home-syncthing-service
                  home-shepherd-service-type
                  (list home-syncthing-shepherd-service)))

(define-public syncthing-services
  (service syncthing-service-type
           (syncthing-configuration
            (arguments '("--no-browser" "--no-default-folder" "--log-max-size=1000")))))

(define-public sway-services
  (list
   (service home-sway-service-type
            (home-sway-configuration
             (config
              `((include ,(local-file "../../configuration/sway/config"))
                ;;; Inputs
                (input * ((xkb_layout "cz")
                          (xkb_variant "dvorak-ucw")
                          (xkb_options "ctrl:nocaps")))
                (input type:touchpad
                       ((tap enabled)
                        (natural_scroll enabled)
                        (dwt enabled)))
                ;;; Outputs
                (set $laptop-monitor "\"eDP-1\"")
                (set $desktop-monitor "\"Dell Inc. DELL U2412M 0FFXD33M4E0S\"")
                ;;(set $tv-monitor "\"Sony SONY TV 0x00000101\"")
                (output "$laptop-monitor" pos 0 0)
                (output "$desktop-monitor" pos 0 -1200)
                (output "$tv-monitor" pos -1360 0)
                ;;; General setup
                (exec dbus-update-activation-environment --system DISPLAY WAYLAND_DISPLAY SWAYSOCK)
                ;;; Theming
                (default_border pixel)
                (default_floating_border pixel)
                (gaps outer 10)
                (gaps inner 10)
                (bar swaybar_command waybar)
                (output * bg "~/.magi/configuration/sway/the-great-wawe-off-kanagawa.jpg" fill)
                ;;; Keybinds
                (set $mod Mod4)
                (set $term alacritty)
                (set $menu "j4-dmenu-desktop --dmenu='bemenu -i --nb \"#3f3f3f\" --nf \"#dcdccc\"j4' --term='alacritty'")
                (bindsym $mod+q kill)
                (bindsym $mod+Return exec $term)
                (bindsym $mod+Space exec "($menu)")
                (bindsym $mod+e exec emacsclient -c)
                (bindsym $mod+w exec nyxt)
                (set $up p)
                (set $down n)
                (set $right f)
                (set $left b)
                ;;; Windows
                (bindsym $mod+$up focus up)
                (bindsym $mod+Up focus up)
                (bindsym $mod+$down focus down)
                (bindsym $mod+Down focus down)
                (bindsym $mod+$right focus right)
                (bindsym $mod+Right focus right)
                (bindsym $mod+$left focus left)
                (bindsym $mod+Left focus left)
                (bindsym $mod+Shift+$up move up)
                (bindsym $mod+Shift+$down move down)
                (bindsym $mod+Shift+$right move right)
                (bindsym $mod+Shift+$left move left)
                (bindsym $mod+v splitv)
                (bindsym $mod+h splith)
                ;;; Workspaces
                (set $ws1 "Development")
                (set $ws2 "Browser")
                (workspace_auto_back_and_forth yes)
                (bindsym $mod+1 workspace $ws1)
                (bindsym $mod+2 workspace $ws2)
                (bindsym $mod+Shift+1 move container to workspace $ws1)
                (bindsym $mod+Shift+2 move container to workspace $ws2)
                (bindsym $mod+Control+Shift+Left move workspace to output $tv-monitor)          
                (bindsym $mod+Control+Shift+Right move workspace to output $laptop-monitor)
                (bindsym $mod+Control+Shift+Up    move workspace to output $desktop-monitor)
                (bindsym $mod+Control+Shift+Down  move workspace to output $laptop-monitor)))))))
                ;;; Windows
                
;; (service home-waybar-service-type
;;          (home-waybar-configuration
;;           (config
;;            `(()))))

(define-public configuration-services
  (list
   (simple-service 'flatpak-vars-service home-environment-variables-service-type
                   `(("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:/var/lib/flatpak/exports/share:$HOME/.local/share/flatpak/exports/share")))
   ;; fixes gnome console bug, remove this once 43 is available
   (simple-service 'fix-nautilus-gnome-console-buf home-environment-variables-service-type
                   `(("NAUTILUS_EXTENSION_PATH" . "")))
   (simple-service 'config-files
                   home-files-service-type
                   `(;;(".config/sway/the-great-wawe-off-kanagawa.jpg" ,(local-file "../../configuration/sway/the-great-wawe-off-kanagawa.jpg"))
                     (".config/waybar/config" ,(local-file "../../configuration/waybar/config"))
                     (".config/waybar/style.css" ,(local-file "../../configuration/waybar/style.css"))
                     (".config/alacritty/alacritty.yml" ,(local-file "../../configuration/alacritty/alacritty.yml"))
                     (".config/nyxt/init.lisp" ,(local-file "../../configuration/nyxt/init.lisp"))))))



(define-public gnupg-services
  (list
   (service home-gnupg-service-type
            (home-gnupg-configuration
             (gpg-config
              (home-gpg-configuration
               (extra-config
                '((cert-digest-algo . "SHA256")
                  (default-preference-list . ("SHA512"
                                              "SHA384"
                                              "SHA256"
                                              "SHA224"
                                              "AES256"
                                              "AES192"
                                              "Uncompressed"))
                  (with-fingerprint? . #t)))))
             (gpg-agent-config
              (home-gpg-agent-configuration
               (pinentry-flavor 'gnome3)
               (extra-options '("--verbose"))
               (extra-config
                '((max-cache-ttl . 86400)))))))))


(define (add-zsh-plugins-load-command packages)
  (home-zsh-extension
   (zshrc
     (map
      (lambda (p)
        (let ((x (package-name p)))
          (mixed-text-file "z"
           "source " p (format #f "/share/zsh/plugins/~a/~a.zsh" x x))))
      packages))))

(define home-zsh-plugin-manager-service-type
  (service-type (name 'home-zsh-plugin-manager)
                (extensions
                 (list (service-extension
                        home-zsh-service-type
                        add-zsh-plugins-load-command)
                       (service-extension
                        home-profile-service-type
                        identity)))
                (compose concatenate)
                (extend append)
                (default-value '())
                (description "\
Install plugins into the home profile and configure Zsh to load them.")))

(define home-zsh-autosuggestions-service-type
  (service-type
   (name 'home-zsh-autosuggestions)
   (extensions
    (list
     (service-extension home-zsh-plugin-manager-service-type list)
     (service-extension
      home-zsh-service-type
      (const
       (home-zsh-extension
        ;; We set variables in zshrc because we need them only in
        ;; interactive shell.
        (zshrc '("# Improve the behavior and perfomance of auto suggestions"
                 "ZSH_AUTOSUGGEST_MANUAL_REBIND=true"
                 "ZSH_AUTOSUGGEST_USE_ASYNC=true"
                 "ZSH_AUTOSUGGEST_STRATEGY=(history completion)")))))))
   (default-value zsh-autosuggestions)
   (description "Enable Fish-like fast and unobtrusive autosuggestions
for Zsh, and set reasonable default values for some plugin's variables
to improve perfomance.")))

(define-public fish-services
  (list home-fish-service-type
        (home-fish-configuration)))

(define-public zsh-services
  (list
   (service home-zsh-plugin-manager-service-type
            (list
             zsh-autosuggestions
             zsh-syntax-highlighting))
   (service home-zsh-service-type
            (home-zsh-configuration
             (xdg-flavor? #t)
             (environment-variables '(("PATH" . "$HOME/.local/bin:$PATH")))
             ;;                          ("LIBRARY_PATH" . "$LIBRARY_PATH:/run/current-system/profile/lib")
             ;;                          ("LD_LIBRARY_PATH" . "$HOME/.config/guix/profile/lib:$HOME/.guix-home/profile/lib:/run/current-system/profile/lib")))
             (zshrc (list (mixed-text-file "z" "
# Improve the behavior and perfomance of auto suggestions"
                           "ZSH_AUTOSUGGEST_MANUAL_REBIND=true"
                           "ZSH_AUTOSUGGEST_USE_ASYNC=true"
                           "ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# Local Variables:
# mode: sh
# End:

# Prevent freezing output on ^s, needed for various isearches
hash stty 2> /dev/null && stty -ixon

# Completions and other stuff
autoload -U compinit
compinit -d ${XDG_CACHE_HOME:-$HOME/.cache}/.zcompdump

# Enable bash completion, requires to source them from somewhere
# autoload -U bashcompinit && bashcompinit

zstyle ':completion:*' menu select
zstyle ':completion:*' insert-tab false

# Automatically update cache of binaries avaliable in $PATH
zstyle ':completion:*' rehash true # Can have a performance penalty

# Approximate completion
# zstyle ':completion:::::' completer _complete _approximate
# zstyle ':completion:*:approximate:*' max-errors 2

# Fuzzy completion
# https://superuser.com/questions/415650/does-a-fuzzy-matching-mode-exist-for-the-zsh-shell
zstyle ':completion:*' matcher-list '' \\
  'm:{a-z\\-}={A-Z\\_}' \\
  'r:[^[:alpha:]]||[[:alpha:]]=** r:|=* m:{a-z\\-}={A-Z\\_}' \\
  'r:|?=** m:{a-z\\-}={A-Z\\_}'

# Make kill completion smart
zstyle ':completion:*:*:*:*:processes' command \"ps -u $USER -o pid,user,args -w -w\"

# Colored completion for files and dirs according to LS_COLORS

hash dircolors 2> /dev/null && eval $(dircolors --sh) && \\
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Prompt theme setup
clear_fn() {
#  zle reset-prompt
  zle kill-buffer
}

prompt_rde_precmd() {
  # Prevent killing prompt on ^C
  trap 'clear_fn' SIGINT
}

prompt_rde_setup() {
  if [[ $UID -eq 0 ]]; then
    user_part='%F{red}>%f'
  else
    user_part='%F{green}>%f'
  fi
  if [ -n \"$GUIX_ENVIRONMENT\" ]; then
    genv_part='%F{blue}>%f'
  fi
  # exit_code_part='%(?..[%?])'

  PS1=\"$user_part$genv_part \"
  # RPS1=\"$exit_code_part\"

  # Fish-like C-c behavior
  # add-zsh-hook precmd prompt_rde_precmd
}

# Load promptinit and set rde theme
autoload -Uz promptinit && promptinit
prompt_themes+=( rde )
prompt rde

setopt printexitvalue # Instead of using RPS1 for status code

echo -en \"\\033[6 q\" # Make a cursor to be a vertical bar

# Remove slashes and dashes from wordchars to make M-b, M-f work
# correctly
WORDCHARS=\"\"

# Configure history
HISTSIZE=5000
SAVEHIST=$HISTSIZE
HISTFILE=${XDG_CACHE_HOME:-$HOME/.cache}/.zhistory

#setopt incappendhistory # Save history to shared file, but not read
setopt sharehistory     # Share history across shell sessions
setopt histignorespace  # Ignore commands that start with space

# Configuring help (M-h to call it on current command/function)
autoload -Uz run-help
(( ${+aliases[run-help]} )) && unalias run-help
autoload -Uz run-help-git

# Delete, home, end buttons
bindkey  \"^[[3~\"  delete-char
bindkey  \"^[[H\"   beginning-of-line
bindkey  \"^[[F\"   end-of-line

# Launch $VISUAL or $EDITOR, for emacsclient if there is no server
# avaliable $ALTERNATE_EDITOR will be used.
autoload -z edit-command-line
zle -N edit-command-line
bindkey \"^X^E\" edit-command-line

alias help=run-help
alias try='guix shell man-db coreutils'
alias ls='ls -p --color=auto'
alias ll='ls -l'
alias grep='grep --color=auto'
")))))))

;; (define-public rainloop
;;   (package
;;    (name "rainloop")
;;    (version "1.17.0")
;;    (source (origin
;;             (method url-fetch)
;;             (uri (string-append "https://github.com/RainLoop/rainloop-webmail/releases/download/v" version "/rainloop-legacy-" version ".zip"))
;;             (sha256
;;              (base32 "02n5d77h11h6vz3glx7rzcn0n14s64ixs0bpdwbxgdfsraxclbbq"))))
;;    (build-system copy-build-system)
;;    (arguments '(#:install-plan
;;                 '(("../data/" "data/")
;;                   ("../index.php" "index.php")
;;                   ("../rainloop/" "rainloop/"))))
;;    (native-inputs (list
;;                    unzip))
;;    (propagated-inputs (list
;;                        php))
;;    (home-page "http://www.rainloop.net/")
;;    (synopsis "")
;;    (description "")
;;    (license license:expat)))

;; (define-configuration/no-serialization rainloop-configuration
;;   (rainloop
;;    (file-like rainloop)
;;    "Rainloop package.")
;;   (runtime-directory
;;    (string "/var/www/rainloop"))
;;   (user
;;    (string "php-fpm"))
;;   (group
;;    (string "php-fpm")))

;; (define (%rainloop-activation config)
;;   (let ((runtime-directory (rainloop-configuration-runtime-directory config))
;;         (rainloop (rainloop-configuration-rainloop config))
;;         (user (rainloop-configuration-user config))
;;         (group (rainloop-configuration-group config)))
;;     #~(begin
;;         (use-modules (guix build utils))
;;         (mkdir-p #$runtime-directory)
;;         (chown #$runtime-directory #$user #$group)
;;         (copy-recursively (string-append #$rainloop "/data") (string-append #$runtime-directory "/data"))
;;         (copy-recursively (string-append #$rainloop "/index.php") (string-append #$runtime-directory "/kindex.php"))
;;         (copy-recursively (string-append #$rainloop "/rainloop") (string-append #$runtime-directory "/rainloop")))))

;; (define-public rainloop-service-type
;;   (service-type
;;    (name 'rainloop)
;;    (description "")
;;    (extensions
;;     (list
;;      (service-extension profile-service-type
;;                         (compose list rainloop-configuration-rainloop))
;;      (service-extension activation-service-type
;;                         %rainloop-activation)))
;;    (default-value (rainloop-configuration))))


