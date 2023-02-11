(define-module (magi home rust)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cargo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-gtk))

(define-public rust-pasts-0.4
  (package
    (name "rust-pasts")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pasts" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "11rdczdhpazclhkbbjafv5nd9ybll9a110crhh67si0p5rdc6mz7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/ardaku/pasts/blob/stable/CHANGELOG.md")
    (synopsis "Minimal and simpler alternative to the futures crate.")
    (description "Minimal and simpler alternative to the futures crate.")
    (license (list license:asl2.0 license:zlib))))
(define-public rust-cala-core-0.1
  (package
    (name "rust-cala-core")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cala_core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "17939zm80lxi0mqsvi98wv2hjasbbh132j5i2m201x30j8dkx4wx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-pasts" ,rust-pasts-0.4)
                       ("rust-stdweb" ,rust-stdweb-0.4)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://github.com/libcala/cala_core/blob/master/CHANGELOG.md")
    (synopsis "Low-level platform glue for Cala")
    (description "Low-level platform glue for Cala")
    (license (list license:asl2.0 license:zlib))))
(define-public rust-whoami-0.9
  (package
    (name "rust-whoami")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "whoami" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "012mw2q72gpmf354yw2qc5w105ziac75shpqp1f62x4hnqx7g13q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cala-core" ,rust-cala-core-0.1))))
    (home-page "https://github.com/ardaku/whoami/blob/stable/CHANGELOG.md")
    (synopsis "Retrieve the current user and environment.")
    (description "Retrieve the current user and environment.")
    (license (list license:expat license:boost1.0))))
(define-public rust-tokio-signal-0.1
  (package
    (name "rust-tokio-signal")
    (version "0.1.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-signal" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1zlz5dwbh8lr0a9zar9459wcbfciqcg74wyiab7hb6hg4dinix78"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures" ,rust-futures-0.1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-mio" ,rust-mio-0.6)
                       ("rust-mio-uds" ,rust-mio-uds-0.6)
                       ("rust-tokio-core" ,rust-tokio-core-0.1)
                       ("rust-tokio-io" ,rust-tokio-io-0.1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/tokio-rs/tokio")
    (synopsis
     "An implementation of an asynchronous Unix signal handling backed futures.
")
    (description
     "An implementation of an asynchronous Unix signal handling backed futures.")
    (license (list license:expat license:asl2.0))))
(define-public rust-rspotify-0.8
  (package
    (name "rust-rspotify")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rspotify" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "14xdic0zhalmvk32y1ffanvgwdqki91qw549kj6mqcdirxka2959"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.10)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-derive-builder" ,rust-derive-builder-0.7)
                       ("rust-dotenv" ,rust-dotenv-0.13)
                       ("rust-env-logger" ,rust-env-logger-0.6)
                       ("rust-failure" ,rust-failure-0.1)
                       ("rust-itertools" ,rust-itertools-0.8)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-percent-encoding" ,rust-percent-encoding-1)
                       ("rust-rand" ,rust-rand-0.6)
                       ("rust-random" ,rust-random-0.12)
                       ("rust-reqwest" ,rust-reqwest-0.10)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-url" ,rust-url-1)
                       ("rust-webbrowser" ,rust-webbrowser-0.5))))
    (home-page "https://github.com/ramsayleung/rspotify")
    (synopsis "Spotify API wrapper")
    (description "Spotify API wrapper")
    (license license:expat)))
(define-public rust-zerocopy-derive-0.1
  (package
    (name "rust-zerocopy-derive")
    (version "0.1.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zerocopy-derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "198g500f9j1z1crq2j6m60jmk0kkmi1x61b4i9p04906rmz4d45h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-0.4)
                       ("rust-syn" ,rust-syn-0.15)
                       ("rust-synstructure" ,rust-synstructure-0.10))))
    (home-page
     "https://fuchsia.googlesource.com/fuchsia/+/HEAD/src/lib/zerocopy/zerocopy-derive")
    (synopsis "Custom derive for traits from the zerocopy crate")
    (description "Custom derive for traits from the zerocopy crate")
    (license license:bsd-3)))
(define-public rust-zerocopy-0.2
  (package
    (name "rust-zerocopy")
    (version "0.2.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zerocopy" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0mj0scnsm2skj3gh3sk8qdn73mj3raw7ky03z5ks3m0gz0qrnawr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-zerocopy-derive" ,rust-zerocopy-derive-0.1))))
    (home-page
     "https://fuchsia.googlesource.com/fuchsia/+/HEAD/src/lib/zerocopy")
    (synopsis "Utilities for zero-copy parsing and serialization")
    (description "Utilities for zero-copy parsing and serialization")
    (license license:bsd-3)))
(define-public rust-unidiff-0.3
  (package
    (name "rust-unidiff")
    (version "0.3.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "unidiff" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0b13vhp2x7jlvmkm44h5niqcxklyrmz6afmppvykp4zimhcjg9nq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/messense/unidiff-rs")
    (synopsis "Unified diff parsing/metadata extraction library for Rust")
    (description "Unified diff parsing/metadata extraction library for Rust")
    (license license:expat)))
(define-public rust-proc-macro2-0.3
  (package
    (name "rust-proc-macro2")
    (version "0.3.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "proc-macro2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1m0ksg6hbm46zblq0dpkwrg3n1h7n90yq1zcgwc6vpbfmr9pr6bp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-unicode-xid" ,rust-unicode-xid-0.1))))
    (home-page "https://github.com/dtolnay/proc-macro2")
    (synopsis
     "A substitute implementation of the compiler's `proc_macro` API to decouple token-based libraries from the procedural macro use case.")
    (description
     "This package provides a substitute implementation of the compiler's `proc_macro`
API to decouple token-based libraries from the procedural macro use case.")
    (license (list license:expat license:asl2.0))))
(define-public rust-bindgen-0.42
  (package
    (name "rust-bindgen")
    (version "0.42.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "bindgen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0p14hgi90wd4zbd8j1s4xrxf770cx0s6a2d32fg9ypmzpb69kwg0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-cexpr" ,rust-cexpr-0.3)
                       ("rust-cfg-if" ,rust-cfg-if-0.1)
                       ("rust-clang-sys" ,rust-clang-sys-0.26)
                       ("rust-clap" ,rust-clap-2)
                       ("rust-env-logger" ,rust-env-logger-0.5)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-peeking-take-while" ,rust-peeking-take-while-0.1)
                       ("rust-proc-macro2" ,rust-proc-macro2-0.3)
                       ("rust-quote" ,rust-quote-0.5)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-which" ,rust-which-1))))
    (home-page "https://rust-lang.github.io/rust-bindgen/")
    (synopsis
     "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (description
     "Automatically generates Rust FFI bindings to C and C++ libraries.")
    (license license:bsd-3)))
(define-public rust-sdl2-sys-0.32
  (package
    (name "rust-sdl2-sys")
    (version "0.32.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sdl2-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "11kz2bqkpcywpyd5hyqflbszpgdmh64zxb61wibpsabx0wji3rrl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.42)
                       ("rust-cfg-if" ,rust-cfg-if-0.1)
                       ("rust-cmake" ,rust-cmake-0.1)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-tar" ,rust-tar-0.4)
                       ("rust-unidiff" ,rust-unidiff-0.3))))
    (home-page "https://github.com/rust-sdl2/rust-sdl2")
    (synopsis "Raw SDL2 bindings for Rust, used internally rust-sdl2")
    (description "Raw SDL2 bindings for Rust, used internally rust-sdl2")
    (license license:expat)))
(define-public rust-c-vec-1
  (package
    (name "rust-c-vec")
    (version "1.3.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "c_vec" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0c3wgb15h97k6lzfm9qgkwk35ij2ad7w8fb5rbqvalyf3n8ii8zq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/GuillaumeGomez/c_vec-rs.git")
    (synopsis "Structures to wrap C arrays")
    (description "Structures to wrap C arrays")
    (license (list license:asl2.0 license:expat))))
(define-public rust-sdl2-0.32
  (package
    (name "rust-sdl2")
    (version "0.32.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sdl2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0yyx7sl08y241ddyyfkk9ysxbxllfdpwny6s37vza0z365ra0lfh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-c-vec" ,rust-c-vec-1)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-num" ,rust-num-0.1)
                       ("rust-rand" ,rust-rand-0.6)
                       ("rust-sdl2-sys" ,rust-sdl2-sys-0.32))))
    (home-page "https://github.com/Rust-SDL2/rust-sdl2")
    (synopsis "SDL2 bindings for Rust")
    (description "SDL2 bindings for Rust")
    (license license:expat)))
(define-public rust-slice-deque-0.3
  (package
    (name "rust-slice-deque")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "slice-deque" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "098gvqjw52qw4gac567c9hx3y6hw9al7hjqb5mnvmvydh3i6xvri"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-mach" ,rust-mach-0.3)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/gnzlbg/slice_deque")
    (synopsis "A double-ended queue that Deref's into a slice.")
    (description
     "This package provides a double-ended queue that Deref's into a slice.")
    (license (list license:expat license:asl2.0))))
(define-public rust-minimp3-sys-0.3
  (package
    (name "rust-minimp3-sys")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "minimp3-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "144vmf3s89kad0smjprzigcp2c9r5dm95n4ydilrbp399irp6772"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/germangb/minimp3-rs.git")
    (synopsis "Rust bindings for the minimp3 library.")
    (description "Rust bindings for the minimp3 library.")
    (license license:expat)))
(define-public rust-minimp3-0.3
  (package
    (name "rust-minimp3")
    (version "0.3.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "minimp3" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1mfj2vg9vx0gvn5qhlsyqifccfynvnxij21mnavgilxzl3vczq6w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-minimp3-sys" ,rust-minimp3-sys-0.3)
                       ("rust-slice-deque" ,rust-slice-deque-0.3))))
    (home-page "https://github.com/germangb/minimp3-rs.git")
    (synopsis "Rust bindings for the minimp3 library.")
    (description "Rust bindings for the minimp3 library.")
    (license license:expat)))
(define-public rust-hound-3
  (package
    (name "rust-hound")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hound" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1cadkxzdsb3bxwzri6r6l78a1jy9j0jxrfwmh34gjadvbnyws4sd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/ruuda/hound")
    (synopsis "A wav encoding and decoding library")
    (description "This package provides a wav encoding and decoding library")
    (license license:asl2.0)))
(define-public rust-claxon-0.4
  (package
    (name "rust-claxon")
    (version "0.4.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "claxon" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1206mxvw833ysg10029apcsjjwly8zmsvksgza5cm7ma4ikzbysb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/ruuda/claxon#readme")
    (synopsis "A FLAC decoding library")
    (description "This package provides a FLAC decoding library")
    (license license:asl2.0)))
(define-public rust-rodio-0.9
  (package
    (name "rust-rodio")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rodio" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0b9s1w55l3zx2ys38yl0gp4k929h065wfl5mlx3x2rjf4ldrc3sx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-claxon" ,rust-claxon-0.4)
                       ("rust-cpal" ,rust-cpal-0.8)
                       ("rust-hound" ,rust-hound-3)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-lewton" ,rust-lewton-0.9)
                       ("rust-minimp3" ,rust-minimp3-0.3)
                       ("rust-nalgebra" ,rust-nalgebra-0.18))))
    (home-page "https://github.com/RustAudio/rodio")
    (synopsis "Audio playback library")
    (description "Audio playback library")
    (license (list license:expat license:asl2.0))))
(define-public rust-portaudio-sys-0.1
  (package
    (name "rust-portaudio-sys")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "portaudio-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xdpywirpr1kqkbak7hnny62gmsc93qgc3ij3j2zskrvjpxa952i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "")
    (synopsis "Bindings to PortAudio")
    (description "Bindings to PortAudio")
    (license license:expat)))
(define-public rust-portaudio-rs-0.3
  (package
    (name "rust-portaudio-rs")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "portaudio-rs" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qnmc7amk0fzbcs985ixv0k4955f0fmpkhrl9ps9pk3cz7pvbdnd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-portaudio-sys" ,rust-portaudio-sys-0.1))))
    (home-page "")
    (synopsis "PortAudio bindings for Rust")
    (description "PortAudio bindings for Rust")
    (license license:expat)))
(define-public rust-linear-map-1
  (package
    (name "rust-linear-map")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "linear-map" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1vh3sczl4xb5asdlpafdf3y4g9bp63fgs8y2a2sjgmcsn7v21bmz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-serde-test" ,rust-serde-test-1))))
    (home-page "https://github.com/contain-rs/linear-map")
    (synopsis "A map implemented by searching linearly in a vector.")
    (description
     "This package provides a map implemented by searching linearly in a vector.")
    (license (list license:expat license:asl2.0))))
(define-public rust-librespot-metadata-0.1
  (package
    (name "rust-librespot-metadata")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "librespot-metadata" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "13dswphpaayccdrvxkj86cfnpv3rj4gl67711gp1qb5j8q7d2hvn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-futures" ,rust-futures-0.1)
                       ("rust-librespot-core" ,rust-librespot-core-0.1)
                       ("rust-librespot-protocol" ,rust-librespot-protocol-0.1)
                       ("rust-linear-map" ,rust-linear-map-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-protobuf" ,rust-protobuf-2))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The metadata logic for librespot")
    (description "The metadata logic for librespot")
    (license license:expat)))
(define-public rust-libpulse-sys-0.0.0
  (package
    (name "rust-libpulse-sys")
    (version "0.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libpulse-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1b74hfq24jzqycfs0ywwvzlkfxvc7r2z8p323c6510zqz831pccv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/jnqnfe/pulse-binding-rust")
    (synopsis "FFI bindings for the PulseAudio libpulse system library.")
    (description "FFI bindings for the PulseAudio libpulse system library.")
    (license #f)))
(define-public rust-jack-sys-0.2
  (package
    (name "rust-jack-sys")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "jack-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1h9c9za19nyr1prx77gkia18ia93f73lpyjdiyrvmhhbs79g54bv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libloading" ,rust-libloading-0.6)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/RustAudio/rust-jack/tree/main/jack-sys")
    (synopsis "Low-level binding to the JACK audio API.")
    (description "Low-level binding to the JACK audio API.")
    (license (list license:expat license:asl2.0))))
(define-public rust-jack-0.5
  (package
    (name "rust-jack")
    (version "0.5.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "jack" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pr2fkjh181b6qjx940vp8hamcadq21p0l7z0nhp8nif5rczq58y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-0.7)
                       ("rust-jack-sys" ,rust-jack-sys-0.2)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/RustAudio/rust-jack")
    (synopsis "Real time audio and midi with JACK.")
    (description "Real time audio and midi with JACK.")
    (license license:expat)))
(define-public rust-gstreamer-base-0.15
  (package
    (name "rust-gstreamer-base")
    (version "0.15.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gstreamer-base" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04hwa85j3w959i025il908bvsx6dyiawkmc0w45hn9kcrisjyma2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-glib" ,rust-glib-0.9)
                       ("rust-glib-sys" ,rust-glib-sys-0.9)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.9)
                       ("rust-gstreamer" ,rust-gstreamer-0.15)
                       ("rust-gstreamer-base-sys" ,rust-gstreamer-base-sys-0.8)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-rustdoc-stripper" ,rust-rustdoc-stripper-0.1))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer Base library")
    (description "Rust bindings for GStreamer Base library")
    (license (list license:expat license:asl2.0))))
(define-public rust-gstreamer-base-sys-0.8
  (package
    (name "rust-gstreamer-base-sys")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gstreamer-base-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1v9v09jqjrwz87c4r7za3yb6g7had112d8zwjdjmhg2b2x94yf5s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.9)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.9)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstbase-1.0")
    (description "FFI bindings to libgstbase-1.0")
    (license license:expat)))
(define-public rust-gstreamer-app-sys-0.8
  (package
    (name "rust-gstreamer-app-sys")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gstreamer-app-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rw1sjbjhlsp31rrkm6l505hsxbzni323dhsfrfwlfy2abhrr1mz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.9)
                       ("rust-gstreamer-base-sys" ,rust-gstreamer-base-sys-0.8)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstapp-1.0")
    (description "FFI bindings to libgstapp-1.0")
    (license license:expat)))
(define-public rust-gstreamer-app-0.15
  (package
    (name "rust-gstreamer-app")
    (version "0.15.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gstreamer-app" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1afnc6kvj7cpiwafdfjn0zfj37nhwbvzjp4n3qgdsnigskl895vq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-sink" ,rust-futures-sink-0.3)
                       ("rust-glib" ,rust-glib-0.9)
                       ("rust-glib-sys" ,rust-glib-sys-0.9)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.9)
                       ("rust-gstreamer" ,rust-gstreamer-0.15)
                       ("rust-gstreamer-app-sys" ,rust-gstreamer-app-sys-0.8)
                       ("rust-gstreamer-base" ,rust-gstreamer-base-0.15)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.8)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-rustdoc-stripper" ,rust-rustdoc-stripper-0.1))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer App library")
    (description "Rust bindings for GStreamer App library")
    (license (list license:expat license:asl2.0))))
(define-public rust-muldiv-0.2
  (package
    (name "rust-muldiv")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "muldiv" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "014jlry2l2ph56mp8knw65637hh49q7fmrraim2bx9vz0a638684"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/sdroege/rust-muldiv")
    (synopsis
     "Provides a trait for numeric types to perform combined multiplication and
division with overflow protection
")
    (description
     "This package provides a trait for numeric types to perform combined
multiplication and division with overflow protection")
    (license license:expat)))
(define-public rust-gstreamer-sys-0.8
  (package
    (name "rust-gstreamer-sys")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gstreamer-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1nsk802vlcyi9p93sg60wv8fzb2mq7j52lfdda4va2kxp40xl60x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-glib-sys" ,rust-glib-sys-0.9)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.9)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "FFI bindings to libgstreamer-1.0")
    (description "FFI bindings to libgstreamer-1.0")
    (license license:expat)))
(define-public rust-gstreamer-0.15
  (package
    (name "rust-gstreamer")
    (version "0.15.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "gstreamer" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qx1fhr9ajms0128ixmi2ncr35llwppdb0z7ximw2vnd2jhn91nf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-cfg-if" ,rust-cfg-if-0.1)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-glib" ,rust-glib-0.9)
                       ("rust-glib-sys" ,rust-glib-sys-0.9)
                       ("rust-gobject-sys" ,rust-gobject-sys-0.9)
                       ("rust-gstreamer-sys" ,rust-gstreamer-sys-0.8)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-muldiv" ,rust-muldiv-0.2)
                       ("rust-num-rational" ,rust-num-rational-0.2)
                       ("rust-paste" ,rust-paste-0.1)
                       ("rust-rustdoc-stripper" ,rust-rustdoc-stripper-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                       ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://gstreamer.freedesktop.org")
    (synopsis "Rust bindings for GStreamer")
    (description "Rust bindings for GStreamer")
    (license (list license:expat license:asl2.0))))
(define-public rust-stdweb-0.1
  (package
    (name "rust-stdweb")
    (version "0.1.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "stdweb" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gjk7ch31a3kgdc39kj4zqinf10yqaf717wanh9kwwbbwg430m7g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-clippy" ,rust-clippy-0.0)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/koute/stdweb")
    (synopsis "A standard library for the client-side Web")
    (description
     "This package provides a standard library for the client-side Web")
    (license (list license:expat license:asl2.0))))
(define-public rust-coreaudio-sys-0.2
  (package
    (name "rust-coreaudio-sys")
    (version "0.2.10")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "coreaudio-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1rjppvvv1j6wbsjw48mrsa5m3z818l5x8f3x0xrp03b3h16l9zrx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.59))))
    (home-page "https://github.com/RustAudio/coreaudio-sys")
    (synopsis
     "Bindings for Apple's CoreAudio frameworks generated via rust-bindgen")
    (description
     "Bindings for Apple's CoreAudio frameworks generated via rust-bindgen")
    (license license:expat)))
(define-public rust-coreaudio-rs-0.9
  (package
    (name "rust-coreaudio-rs")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "coreaudio-rs" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "14g4yqsbhif2bqdk4qk0lixfy78gl1p8lrl122qyklysclcpcagj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-coreaudio-sys" ,rust-coreaudio-sys-0.2))))
    (home-page "https://github.com/RustAudio/coreaudio-rs")
    (synopsis "A friendly rust interface for Apple's CoreAudio API.")
    (description
     "This package provides a friendly rust interface for Apple's CoreAudio API.")
    (license (list license:expat license:asl2.0))))
(define-public rust-core-foundation-sys-0.5
  (package
    (name "rust-core-foundation-sys")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "core-foundation-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1yiyi30bnlnh29i21gp5f411b4qaj05vc8zp8j1y9b0khqg2fv3i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Foundation for macOS")
    (description "Bindings to Core Foundation for macOS")
    (license (list license:expat license:asl2.0))))
(define-public rust-cpal-0.8
  (package
    (name "rust-cpal")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cpal" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "17l298jyp4lanl0igxp30m6xnv84gacvdbp3ylrv5c9ncpny32nm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-alsa-sys" ,rust-alsa-sys-0.1)
                       ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.5)
                       ("rust-coreaudio-rs" ,rust-coreaudio-rs-0.9)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-stdweb" ,rust-stdweb-0.1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/rustaudio/cpal")
    (synopsis "Low-level cross-platform audio I/O library in pure Rust.")
    (description "Low-level cross-platform audio I/O library in pure Rust.")
    (license license:asl2.0)))
(define-public rust-nix-0.9
  (package
    (name "rust-nix")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "nix" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0cpvygha6bak7apkp9cm5snmld3lnm26crlxavl7pv4q07mszid2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-0.9)
                       ("rust-cfg-if" ,rust-cfg-if-0.1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-void" ,rust-void-1))))
    (home-page "https://github.com/nix-rust/nix")
    (synopsis "Rust friendly bindings to *nix APIs")
    (description "Rust friendly bindings to *nix APIs")
    (license license:expat)))
(define-public rust-alsa-0.2
  (package
    (name "rust-alsa")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "alsa" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0cpb9bz4r4x564gp0yclb1589bab7ghsxjcvvv2l2c5jr3mx985l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-alsa-sys" ,rust-alsa-sys-0.1)
                       ("rust-bitflags" ,rust-bitflags-0.9)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-nix" ,rust-nix-0.9))))
    (home-page "https://github.com/diwic/alsa-rs")
    (synopsis "Thin but safe wrappers for ALSA (Linux sound API)")
    (description "Thin but safe wrappers for ALSA (Linux sound API)")
    (license (list license:asl2.0 license:expat))))
(define-public rust-librespot-playback-0.1
  (package
    (name "rust-librespot-playback")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "librespot-playback" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1mpczkj967wv51srcjlqaz3r9b2lyqdmn62cd4n1bxgcidhwlj8j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-alsa" ,rust-alsa-0.2)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-cpal" ,rust-cpal-0.8)
                       ("rust-futures" ,rust-futures-0.1)
                       ("rust-glib" ,rust-glib-0.9)
                       ("rust-gstreamer" ,rust-gstreamer-0.15)
                       ("rust-gstreamer-app" ,rust-gstreamer-app-0.15)
                       ("rust-jack" ,rust-jack-0.5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libpulse-sys" ,rust-libpulse-sys-0.0.0)
                       ("rust-librespot-audio" ,rust-librespot-audio-0.1)
                       ("rust-librespot-core" ,rust-librespot-core-0.1)
                       ("rust-librespot-metadata" ,rust-librespot-metadata-0.1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-portaudio-rs" ,rust-portaudio-rs-0.3)
                       ("rust-rodio" ,rust-rodio-0.9)
                       ("rust-sdl2" ,rust-sdl2-0.32)
                       ("rust-shell-words" ,rust-shell-words-0.1)
                       ("rust-zerocopy" ,rust-zerocopy-0.2))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The audio playback logic for librespot")
    (description "The audio playback logic for librespot")
    (license license:expat)))
(define-public rust-if-addrs-sys-0.3
  (package
    (name "rust-if-addrs-sys")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "if-addrs-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1skrzs79rafv185064p44r0k1va9ig4bfnpbwlvyhxh4g3fvjx6y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/messense/if-addrs")
    (synopsis "if_addrs sys crate")
    (description "if_addrs sys crate")
    (license (list license:expat license:bsd-3))))
(define-public rust-if-addrs-0.6
  (package
    (name "rust-if-addrs")
    (version "0.6.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "if-addrs" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pkkkwm9znn07xq9s6glf8lxzn2rdxvy8kwkw6czrw64ywhy8wr2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-if-addrs-sys" ,rust-if-addrs-sys-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/messense/if-addrs")
    (synopsis "Return interface IP addresses on Posix and windows systems")
    (description "Return interface IP addresses on Posix and windows systems")
    (license (list license:expat license:bsd-3))))
(define-public rust-libmdns-0.2
  (package
    (name "rust-libmdns")
    (version "0.2.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libmdns" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0kirdfq8i9r5irnxrhsnhc0pqli4kdqar0n47dim6v3kfk0q51ax"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-futures" ,rust-futures-0.1)
                       ("rust-hostname" ,rust-hostname-0.3)
                       ("rust-if-addrs" ,rust-if-addrs-0.6)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-multimap" ,rust-multimap-0.8)
                       ("rust-net2" ,rust-net2-0.2)
                       ("rust-quick-error" ,rust-quick-error-1)
                       ("rust-rand" ,rust-rand-0.7)
                       ("rust-tokio-core" ,rust-tokio-core-0.1))))
    (home-page "https://github.com/librespot-org/libmdns")
    (synopsis
     "mDNS Responder library for building discoverable LAN services in Rust")
    (description
     "mDNS Responder library for building discoverable LAN services in Rust")
    (license license:expat)))
(define-public rust-dns-sd-0.1
  (package
    (name "rust-dns-sd")
    (version "0.1.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dns-sd" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "11r0jymjshfnn3sh2nqjhrikk4r5rr1g36sip9iqy8i0xafm0j6p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/plietar/rust-dns-sd")
    (synopsis "Rust binding for dns-sd")
    (description "Rust binding for dns-sd")
    (license license:expat)))
(define-public rust-block-modes-0.3
  (package
    (name "rust-block-modes")
    (version "0.3.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "block-modes" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0af562hvwgbvn38npmrw5rybvma819rvb7wh6avzsfay14889aii"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-block-cipher-trait" ,rust-block-cipher-trait-0.6)
                       ("rust-block-padding" ,rust-block-padding-0.1))))
    (home-page "")
    (synopsis
     "This crate is deprecated. Use crates from https://github.com/RustCrypto/block-modes instead.")
    (description "This crate is deprecated.  Use crates from
https://github.com/RustCrypto/block-modes instead.")
    (license (list license:expat license:asl2.0))))
(define-public rust-librespot-connect-0.1
  (package
    (name "rust-librespot-connect")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "librespot-connect" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1r82z5ggs5rsih7kch5kpdyb0ajjb8h88fkg97r5na6xy23mmapx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aes-ctr" ,rust-aes-ctr-0.3)
                       ("rust-base64" ,rust-base64-0.10)
                       ("rust-block-modes" ,rust-block-modes-0.3)
                       ("rust-dns-sd" ,rust-dns-sd-0.1)
                       ("rust-futures" ,rust-futures-0.1)
                       ("rust-hmac" ,rust-hmac-0.7)
                       ("rust-hyper" ,rust-hyper-0.11)
                       ("rust-libmdns" ,rust-libmdns-0.2)
                       ("rust-librespot-core" ,rust-librespot-core-0.1)
                       ("rust-librespot-playback" ,rust-librespot-playback-0.1)
                       ("rust-librespot-protocol" ,rust-librespot-protocol-0.1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-num-bigint" ,rust-num-bigint-0.2)
                       ("rust-protobuf" ,rust-protobuf-2)
                       ("rust-rand" ,rust-rand-0.7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha-1" ,rust-sha-1-0.8)
                       ("rust-tokio-core" ,rust-tokio-core-0.1)
                       ("rust-url" ,rust-url-1))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The discovery and Spotify Connect logic for librespot")
    (description "The discovery and Spotify Connect logic for librespot")
    (license license:expat)))
(define-public rust-vorbisfile-sys-0.0.8
  (package
    (name "rust-vorbisfile-sys")
    (version "0.0.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "vorbisfile-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1la2j2zbzdjd93byz21ij58c540bfn1r9pi0bssrjimcw7bhchsg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gcc" ,rust-gcc-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-ogg-sys" ,rust-ogg-sys-0.0.9)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-vorbis-sys" ,rust-vorbis-sys-0.1))))
    (home-page "")
    (synopsis "FFI for the vorbisfile library")
    (description "FFI for the vorbisfile library")
    (license license:expat)))
(define-public rust-vorbis-sys-0.1
  (package
    (name "rust-vorbis-sys")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "vorbis-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1zgv7lwa4b2z091g25h83zil8bawk4frc1f0ril5xa31agpxd7mx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-ogg-sys" ,rust-ogg-sys-0.0.9)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "")
    (synopsis "FFI for the libvorbis library")
    (description "FFI for the libvorbis library")
    (license license:expat)))
(define-public rust-vorbis-0.0.14
  (package
    (name "rust-vorbis")
    (version "0.0.14")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "vorbis" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0xn7diq8qz2zmsmwzg3rcsxmpmm2gj7wgnl2gdan0lq7ax21k2jy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-ogg-sys" ,rust-ogg-sys-0.0.9)
                       ("rust-vorbis-sys" ,rust-vorbis-sys-0.1)
                       ("rust-vorbisfile-sys" ,rust-vorbisfile-sys-0.0.8))))
    (home-page "https://github.com/tomaka/vorbis-rs")
    (synopsis "High-level bindings for the official libvorbis library.")
    (description "High-level bindings for the official libvorbis library.")
    (license license:asl2.0)))
(define-public rust-ogg-sys-0.0.9
  (package
    (name "rust-ogg-sys")
    (version "0.0.9")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ogg-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1cpx6n5ndh2d59g43l6rj3myzi5jsc0n6rldpx0impqp5qbqqnx9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-gcc" ,rust-gcc-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/tomaka/ogg-sys")
    (synopsis "FFI for libogg, the media container.")
    (description "FFI for libogg, the media container.")
    (license license:expat)))
(define-public rust-librespot-tremor-0.2
  (package
    (name "rust-librespot-tremor")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "librespot-tremor" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1zmld16zawvn7ayrf318lwdr2d7awn4bk9s0d6kpim0mz6zjbxcp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-ogg-sys" ,rust-ogg-sys-0.0.9)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "")
    (synopsis "Rust bindings to tremor")
    (description "Rust bindings to tremor")
    (license license:expat)))
(define-public rust-shannon-0.2
  (package
    (name "rust-shannon")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "shannon" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qa52zs4y1i87ysr11g9p6shpdagl14bb340gfm6rd97jhfb99by"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1))))
    (home-page "")
    (synopsis "Shannon cipher implementation")
    (description "Shannon cipher implementation")
    (license license:expat)))
(define-public rust-protobuf-codegen-pure-2
  (package
    (name "rust-protobuf-codegen-pure")
    (version "2.14.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "protobuf-codegen-pure" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0h34gfqlb7bqmgqv1mfgy5wk35z5r2h5ki3p3pdcmw1vqzmly6id"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-protobuf" ,rust-protobuf-2)
                       ("rust-protobuf-codegen" ,rust-protobuf-codegen-2))))
    (home-page
     "https://github.com/stepancheg/rust-protobuf/tree/master/protobuf-codegen-pure/")
    (synopsis "Pure-rust codegen for protobuf using protobuf-parser crate

WIP
")
    (description "Pure-rust codegen for protobuf using protobuf-parser crate

WIP")
    (license license:expat)))
(define-public rust-protobuf-codegen-2
  (package
    (name "rust-protobuf-codegen")
    (version "2.14.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "protobuf-codegen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "031bx325lsgcx7wc76vc2cqph6q0b34jgc8nz0g2rkwcfnx3n4fy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-protobuf" ,rust-protobuf-2))))
    (home-page "https://github.com/stepancheg/rust-protobuf/")
    (synopsis
     "Code generator for rust-protobuf.

Includes a library to invoke programmatically (e. g. from `build.rs`) and `protoc-gen-rust` binary.
")
    (description
     "Code generator for rust-protobuf.

Includes a library to invoke programmatically (e.  g.  from `build.rs`) and
`protoc-gen-rust` binary.")
    (license license:expat)))
(define-public rust-protobuf-2
  (package
    (name "rust-protobuf")
    (version "2.14.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "protobuf" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "11bl8hf522s9mbkckivnn9n8s3ss4g41w6jmfdsswmr5adqd71lf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-0.5)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://github.com/stepancheg/rust-protobuf/")
    (synopsis "Rust implementation of Google protocol buffers
")
    (description "Rust implementation of Google protocol buffers")
    (license license:expat)))
(define-public rust-librespot-protocol-0.1
  (package
    (name "rust-librespot-protocol")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "librespot-protocol" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1n8gr8g4yrjp6i0rbsm7vpq00x9yh3lbjkzy1m5d8sgkkss048yl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-glob" ,rust-glob-0.3)
                       ("rust-protobuf" ,rust-protobuf-2)
                       ("rust-protobuf-codegen" ,rust-protobuf-codegen-2)
                       ("rust-protobuf-codegen-pure" ,rust-protobuf-codegen-pure-2))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The protobuf logic for communicating with Spotify servers")
    (description "The protobuf logic for communicating with Spotify servers")
    (license license:expat)))
(define-public rust-tokio-tls-0.1
  (package
    (name "rust-tokio-tls")
    (version "0.1.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tokio-tls" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04yrdscn8m9qza8ms09pqipbmj6x2q64jgm5n3ipy4b0wl24nbvp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures" ,rust-futures-0.1)
                       ("rust-native-tls" ,rust-native-tls-0.1)
                       ("rust-tokio-core" ,rust-tokio-core-0.1)
                       ("rust-tokio-io" ,rust-tokio-io-0.1)
                       ("rust-tokio-proto" ,rust-tokio-proto-0.1))))
    (home-page "https://tokio.rs")
    (synopsis
     "Deprecated in favor of `tokio-naitve-tls`.

An implementation of TLS/SSL streams for Tokio giving an implementation of TLS
for nonblocking I/O streams.
")
    (description
     "Deprecated in favor of `tokio-naitve-tls`.

An implementation of TLS/SSL streams for Tokio giving an implementation of TLS
for nonblocking I/O streams.")
    (license (list license:expat license:asl2.0))))
(define-public rust-hyper-tls-0.1
  (package
    (name "rust-hyper-tls")
    (version "0.1.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hyper-tls" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0y13a98grzvgza9wpql0gmghwhp48jzxn5dk1a26ac4da5gbvcgz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures" ,rust-futures-0.1)
                       ("rust-hyper" ,rust-hyper-0.11)
                       ("rust-native-tls" ,rust-native-tls-0.1)
                       ("rust-tokio-core" ,rust-tokio-core-0.1)
                       ("rust-tokio-io" ,rust-tokio-io-0.1)
                       ("rust-tokio-service" ,rust-tokio-service-0.1)
                       ("rust-tokio-tls" ,rust-tokio-tls-0.1))))
    (home-page "https://hyper.rs")
    (synopsis "Default TLS implementation for use with hyper")
    (description "Default TLS implementation for use with hyper")
    (license (list license:expat license:asl2.0))))
(define-public rust-hyper-proxy-0.4
  (package
    (name "rust-hyper-proxy")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hyper-proxy" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1kmy7xybj9sdw2162bzhcxap93syq89d4za7dqg4hzklw9fr5w24"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-0.4)
                       ("rust-futures" ,rust-futures-0.1)
                       ("rust-hyper" ,rust-hyper-0.11)
                       ("rust-hyper-tls" ,rust-hyper-tls-0.1)
                       ("rust-native-tls" ,rust-native-tls-0.1)
                       ("rust-tokio-core" ,rust-tokio-core-0.1)
                       ("rust-tokio-io" ,rust-tokio-io-0.1)
                       ("rust-tokio-tls" ,rust-tokio-tls-0.1))))
    (home-page "https://github.com/tafia/hyper-proxy")
    (synopsis "A proxy connector for Hyper-based applications")
    (description
     "This package provides a proxy connector for Hyper-based applications")
    (license license:expat)))
(define-public rust-librespot-core-0.1
  (package
    (name "rust-librespot-core")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "librespot-core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02mzh76xx014bvffbzqnybr5s8c7ycyphg1qn71c6lhiyvqay7i7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aes" ,rust-aes-0.3)
                       ("rust-base64" ,rust-base64-0.10)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-0.4)
                       ("rust-error-chain" ,rust-error-chain-0.12)
                       ("rust-futures" ,rust-futures-0.1)
                       ("rust-hmac" ,rust-hmac-0.7)
                       ("rust-httparse" ,rust-httparse-1)
                       ("rust-hyper" ,rust-hyper-0.11)
                       ("rust-hyper-proxy" ,rust-hyper-proxy-0.4)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-librespot-protocol" ,rust-librespot-protocol-0.1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-num-bigint" ,rust-num-bigint-0.2)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-pbkdf2" ,rust-pbkdf2-0.3)
                       ("rust-protobuf" ,rust-protobuf-2)
                       ("rust-rand" ,rust-rand-0.7)
                       ("rust-rand" ,rust-rand-0.7)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha-1" ,rust-sha-1-0.8)
                       ("rust-shannon" ,rust-shannon-0.2)
                       ("rust-tokio-codec" ,rust-tokio-codec-0.1)
                       ("rust-tokio-core" ,rust-tokio-core-0.1)
                       ("rust-tokio-io" ,rust-tokio-io-0.1)
                       ("rust-url" ,rust-url-1)
                       ("rust-uuid" ,rust-uuid-0.7)
                       ("rust-vergen" ,rust-vergen-3))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The core functionality provided by librespot")
    (description "The core functionality provided by librespot")
    (license license:expat)))
(define-public rust-ogg-0.7
  (package
    (name "rust-ogg")
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ogg" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ldnq9hccrsaqyzp6yb6w2nn1mpd4wd5fqsckmrf3ybsa71p3r8k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-0.4)
                       ("rust-futures" ,rust-futures-0.1)
                       ("rust-tokio-io" ,rust-tokio-io-0.1))))
    (home-page "https://github.com/RustAudio/ogg")
    (synopsis "Ogg container decoder and encoder written in pure Rust")
    (description "Ogg container decoder and encoder written in pure Rust")
    (license license:bsd-3)))
(define-public rust-lewton-0.9
  (package
    (name "rust-lewton")
    (version "0.9.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "lewton" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1l4bc88cpr8p94dfycykn8gajg20kp611kx159fc8dkh64d2qm4d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-futures" ,rust-futures-0.1)
                       ("rust-ogg" ,rust-ogg-0.7)
                       ("rust-smallvec" ,rust-smallvec-0.6)
                       ("rust-tokio-io" ,rust-tokio-io-0.1))))
    (home-page "https://github.com/RustAudio/lewton")
    (synopsis "Pure Rust vorbis decoder")
    (description "Pure Rust vorbis decoder")
    (license (list license:expat license:asl2.0))))
(define-public rust-ctr-0.3
  (package
    (name "rust-ctr")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ctr" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0dk7ik2pjr10q8b3bm64af10k0x0xkl6y02xs9kxz4a4f28xcb02"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-block-cipher-trait" ,rust-block-cipher-trait-0.6)
                       ("rust-stream-cipher" ,rust-stream-cipher-0.3))))
    (home-page "https://github.com/RustCrypto/block-modes")
    (synopsis "CTR block modes of operation")
    (description "CTR block modes of operation")
    (license (list license:expat license:asl2.0))))
(define-public rust-aes-ctr-0.3
  (package
    (name "rust-aes-ctr")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "aes-ctr" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1vnib0h0ala3dg07iks60vwi13jgar33j3wc3l6sxgm3ir2v1rfj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aes-soft" ,rust-aes-soft-0.3)
                       ("rust-aesni" ,rust-aesni-0.6)
                       ("rust-ctr" ,rust-ctr-0.3)
                       ("rust-stream-cipher" ,rust-stream-cipher-0.3))))
    (home-page "https://github.com/RustCrypto/block-ciphers/tree/master/aes")
    (synopsis "DEPRECATED: replaced by the `aes` crate")
    (description "DEPRECATED: replaced by the `aes` crate")
    (license (list license:expat license:asl2.0))))
(define-public rust-librespot-audio-0.1
  (package
    (name "rust-librespot-audio")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "librespot-audio" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1kpk83m67ahb65q4h1f2b0k4pa1rdlj73gxfmpzdrn4c2m0icr4l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aes-ctr" ,rust-aes-ctr-0.3)
                       ("rust-bit-set" ,rust-bit-set-0.5)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bytes" ,rust-bytes-0.4)
                       ("rust-futures" ,rust-futures-0.1)
                       ("rust-lewton" ,rust-lewton-0.9)
                       ("rust-librespot-core" ,rust-librespot-core-0.1)
                       ("rust-librespot-tremor" ,rust-librespot-tremor-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-num-bigint" ,rust-num-bigint-0.2)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-vorbis" ,rust-vorbis-0.0.14))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis "The audio fetching logic for librespot")
    (description "The audio fetching logic for librespot")
    (license license:expat)))
(define-public rust-librespot-0.1
  (package
    (name "rust-librespot")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "librespot" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rn53hamcbknh3k1h9l5cy62gq78wwsq915xqng8ddr0x8ny575b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.10)
                       ("rust-env-logger" ,rust-env-logger-0.6)
                       ("rust-futures" ,rust-futures-0.1)
                       ("rust-getopts" ,rust-getopts-0.2)
                       ("rust-hex" ,rust-hex-0.3)
                       ("rust-hyper" ,rust-hyper-0.11)
                       ("rust-librespot-audio" ,rust-librespot-audio-0.1)
                       ("rust-librespot-connect" ,rust-librespot-connect-0.1)
                       ("rust-librespot-core" ,rust-librespot-core-0.1)
                       ("rust-librespot-metadata" ,rust-librespot-metadata-0.1)
                       ("rust-librespot-playback" ,rust-librespot-playback-0.1)
                       ("rust-librespot-protocol" ,rust-librespot-protocol-0.1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-num-bigint" ,rust-num-bigint-0.2)
                       ("rust-protobuf" ,rust-protobuf-2)
                       ("rust-rand" ,rust-rand-0.7)
                       ("rust-rpassword" ,rust-rpassword-3)
                       ("rust-sha-1" ,rust-sha-1-0.8)
                       ("rust-tokio-core" ,rust-tokio-core-0.1)
                       ("rust-tokio-io" ,rust-tokio-io-0.1)
                       ("rust-tokio-process" ,rust-tokio-process-0.2)
                       ("rust-tokio-signal" ,rust-tokio-signal-0.2)
                       ("rust-url" ,rust-url-1))))
    (home-page "https://github.com/librespot-org/librespot")
    (synopsis
     "An open source client library for Spotify, with support for Spotify Connect")
    (description
     "An open source client library for Spotify, with support for Spotify Connect")
    (license license:expat)))
(define-public rust-security-framework-sys-2
  (package
    (name "rust-security-framework-sys")
    (version "2.6.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "security-framework-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0mn5lm0jip9nm6ydqm6qd9alyiwq15c027777jsbyibs2wxa2q01"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://lib.rs/crates/security-framework-sys")
    (synopsis "Apple `Security.framework` low-level FFI bindings")
    (description "Apple `Security.framework` low-level FFI bindings")
    (license (list license:expat license:asl2.0))))
(define-public rust-libc-0.2
  (package
    (name "rust-libc")
    (version "0.2.132")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "199vm5mz5gmd73lx07g06g2d9kl1qrd4dcky2bdrcfhw6kjy8wc3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.
")
    (description "Raw FFI bindings to platform libraries like libc.")
    (license (list license:expat license:asl2.0))))
(define-public rust-core-foundation-sys-0.8
  (package
    (name "rust-core-foundation-sys")
    (version "0.8.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "core-foundation-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1p5r2wckarkpkyc4z83q08dwpvcafrb1h6fxfa3qnikh8szww9sq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Foundation for macOS")
    (description "Bindings to Core Foundation for macOS")
    (license (list license:expat license:asl2.0))))
(define-public rust-core-foundation-0.9
  (package
    (name "rust-core-foundation")
    (version "0.9.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "core-foundation" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ii1ihpjb30fk38gdikm5wqlkmyr8k46fh4k2r8sagz5dng7ljhr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-uuid" ,rust-uuid-0.5))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Foundation for macOS")
    (description "Bindings to Core Foundation for macOS")
    (license (list license:expat license:asl2.0))))
(define-public rust-security-framework-2
  (package
    (name "rust-security-framework")
    (version "2.7.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "security-framework" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0v1m0vchbibfr1l0pqiyscp0y7h7f7vkjmy52cc67xjah2bvph9b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-core-foundation" ,rust-core-foundation-0.9)
                       ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-security-framework-sys" ,rust-security-framework-sys-2))))
    (home-page "https://lib.rs/crates/security_framework")
    (synopsis "Security.framework bindings for macOS and iOS")
    (description "Security.framework bindings for macOS and iOS")
    (license (list license:expat license:asl2.0))))
(define-public rust-zvariant-derive-2
  (package
    (name "rust-zvariant-derive")
    (version "2.10.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zvariant_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1s9xk9c4p9vl0j2vr1abqc12mgv500sjc3fnh8ij3d1yb4i5xjp4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro-crate" ,rust-proc-macro-crate-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://gitlab.freedesktop.org/dbus/zbus/")
    (synopsis "D-Bus & GVariant encoding & decoding")
    (description "D-Bus & GVariant encoding & decoding")
    (license license:expat)))
(define-public rust-zvariant-2
  (package
    (name "rust-zvariant")
    (version "2.10.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zvariant" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0995d59vl8409mk3qrbshqrz5d76dq52szg0x2vqji07y9app356"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.5)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-enumflags2" ,rust-enumflags2-0.6)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                       ("rust-static-assertions" ,rust-static-assertions-1)
                       ("rust-zvariant-derive" ,rust-zvariant-derive-2))))
    (home-page "https://gitlab.freedesktop.org/dbus/zbus/")
    (synopsis "D-Bus & GVariant encoding & decoding")
    (description "D-Bus & GVariant encoding & decoding")
    (license license:expat)))
(define-public rust-zbus-macros-1
  (package
    (name "rust-zbus-macros")
    (version "1.9.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zbus_macros" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "19p0pdwdf52zkaknav0pj5qvgcf52xk8a4p3a4ymxybwhjkmjfgs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro-crate" ,rust-proc-macro-crate-0.1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://gitlab.freedesktop.org/dbus/zbus/")
    (synopsis "proc-macros for zbus")
    (description "proc-macros for zbus")
    (license license:expat)))
(define-public rust-nix-0.22
  (package
    (name "rust-nix")
    (version "0.22.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "nix" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1bsgc8vjq07a1wg9vz819bva3dvn58an4r87h80dxrfqkqanz4g4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memoffset" ,rust-memoffset-0.6))))
    (home-page "https://github.com/nix-rust/nix")
    (synopsis "Rust friendly bindings to *nix APIs")
    (description "Rust friendly bindings to *nix APIs")
    (license license:expat)))
(define-public rust-zbus-1
  (package
    (name "rust-zbus")
    (version "1.9.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zbus" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0jgwydwjgk16dyrzdbc1k0dnqj9kv9p3fwcv92a7l9np3hlv5glw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-io" ,rust-async-io-1)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-derivative" ,rust-derivative-2)
                       ("rust-enumflags2" ,rust-enumflags2-0.6)
                       ("rust-fastrand" ,rust-fastrand-1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-nb-connect" ,rust-nb-connect-1)
                       ("rust-nix" ,rust-nix-0.22)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-polling" ,rust-polling-2)
                       ("rust-scoped-tls" ,rust-scoped-tls-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-xml-rs" ,rust-serde-xml-rs-0.4)
                       ("rust-serde-repr" ,rust-serde-repr-0.1)
                       ("rust-zbus-macros" ,rust-zbus-macros-1)
                       ("rust-zvariant" ,rust-zvariant-2))))
    (home-page "https://gitlab.freedesktop.org/dbus/zbus/")
    (synopsis "API for D-Bus communication")
    (description "API for D-Bus communication")
    (license license:expat)))
(define-public rust-secret-service-2
  (package
    (name "rust-secret-service")
    (version "2.0.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "secret-service" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18l0yz9sb062jddcx56qi70d4ry2js3irkgysdgii0w77d15rnp1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aes" ,rust-aes-0.7)
                       ("rust-block-modes" ,rust-block-modes-0.8)
                       ("rust-hkdf" ,rust-hkdf-0.11)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-num" ,rust-num-0.4)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha2" ,rust-sha2-0.9)
                       ("rust-zbus" ,rust-zbus-1)
                       ("rust-zbus-macros" ,rust-zbus-macros-1)
                       ("rust-zvariant" ,rust-zvariant-2)
                       ("rust-zvariant-derive" ,rust-zvariant-derive-2))))
    (home-page "https://github.com/hwchen/secret-service-rs.git")
    (synopsis "Library to interface with Secret Service API")
    (description "Library to interface with Secret Service API")
    (license (list license:expat license:asl2.0))))
(define-public rust-keyring-0.10
  (package
    (name "rust-keyring")
    (version "0.10.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "keyring" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0xs73nygvd6gb5mnisdxngqdh0i5vmbg0id8k1l0nfv6d8aqp6m4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-secret-service" ,rust-secret-service-2)
                       ("rust-security-framework" ,rust-security-framework-2)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/hwchen/keyring-rs")
    (synopsis "Cross-platform library for managing passwords/credentials")
    (description "Cross-platform library for managing passwords/credentials")
    (license (list license:expat license:asl2.0))))
(define-public rust-dbus-tokio-0.2
  (package
    (name "rust-dbus-tokio")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dbus-tokio" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1b87p6gzfycl2hrpcsnv7dak93m1nrmiyirh02g3mmmk7sjqm9f4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dbus" ,rust-dbus-0.6)
                       ("rust-futures" ,rust-futures-0.1)
                       ("rust-log" ,rust-log-0.3)
                       ("rust-mio" ,rust-mio-0.6)
                       ("rust-tokio-core" ,rust-tokio-core-0.1))))
    (home-page "https://github.com/diwic/dbus-rs")
    (synopsis
     "Makes it possible to use Tokio with D-Bus, which is a bus commonly used on Linux for inter-process communication.")
    (description
     "Makes it possible to use Tokio with D-Bus, which is a bus commonly used on Linux
for inter-process communication.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-tracing-error-0.1
  (package
    (name "rust-tracing-error")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tracing-error" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "092y3357af6058mdw7nmr7sysqdka8b4cyaqz940fl2a7nwc1mxl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.2))))
    (home-page "https://tokio.rs")
    (synopsis "Utilities for enriching errors with `tracing`.
")
    (description "Utilities for enriching errors with `tracing`.")
    (license license:expat)))
(define-public rust-owo-colors-1
  (package
    (name "rust-owo-colors")
    (version "1.4.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "owo-colors" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rybl2lvhaycpkpaq45099idp5ny7nv4sqsafz0cvfqw1wjfy9vz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atty" ,rust-atty-0.2))))
    (home-page "https://github.com/jam1garner/owo-colors")
    (synopsis "Zero-allocation terminal colors that'll make people go owo")
    (description "Zero-allocation terminal colors that'll make people go owo")
    (license license:expat)))
(define-public rust-color-spantrace-0.1
  (package
    (name "rust-color-spantrace")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "color-spantrace" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1lb2li71zvpxp80nck98gcqbqm3dnmp43pnlvm52z9x8livy9vmn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-once-cell" ,rust-once-cell-1)
                       ("rust-owo-colors" ,rust-owo-colors-1)
                       ("rust-tracing-core" ,rust-tracing-core-0.1)
                       ("rust-tracing-error" ,rust-tracing-error-0.1))))
    (home-page "https://github.com/yaahc/color-spantrace")
    (synopsis
     "A pretty printer for tracing_error::SpanTrace based on color-backtrace")
    (description
     "This package provides a pretty printer for tracing_error::SpanTrace based on
color-backtrace")
    (license (list license:expat license:asl2.0))))
(define-public rust-color-eyre-0.5
  (package
    (name "rust-color-eyre")
    (version "0.5.11")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "color-eyre" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1dspj58bk57f9hiqlvbz25rik92i4a95iwa2dl4pg8g8grlqa60z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-color-spantrace" ,rust-color-spantrace-0.1)
                       ("rust-eyre" ,rust-eyre-0.6)
                       ("rust-indenter" ,rust-indenter-0.3)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-owo-colors" ,rust-owo-colors-1)
                       ("rust-tracing-error" ,rust-tracing-error-0.1)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/yaahc/color-eyre")
    (synopsis
     "An error report handler for panics and eyre::Reports for colorful, consistent, and well formatted error reports for all kinds of errors.")
    (description
     "An error report handler for panics and eyre::Reports for colorful, consistent,
and well formatted error reports for all kinds of errors.")
    (license (list license:expat license:asl2.0))))
(define-public rust-alsa-sys-0.1
  (package
    (name "rust-alsa-sys")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "alsa-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0n3xr2msblmqlsx313b2y2v9hamqh0hp43v23fp1b3znkszwpvdh"))))
    (build-system cargo-build-system)
    (native-inputs (list pkg-config))
    (inputs (list alsa-lib))
    (arguments
     (list #:skip-build? #t
           #:cargo-inputs `(("rust-libc" ,rust-libc-0.2)
                            ("rust-pkg-config" ,rust-pkg-config-0.3))))
    ;; (search-paths
    ;;  (list
    ;;   #~(search-path-specification
    ;;    (variable "PKG_CONFIG_PATH")
    ;;    (files (list
    ;;            (string-append #$alsa-lib "/lib/pkgconfig/"))))))
    (home-page "https://github.com/diwic/alsa-sys")
    (synopsis
     "FFI bindings for the ALSA project (Advanced Linux Sound Architecture)")
    (description
     "FFI bindings for the ALSA project (Advanced Linux Sound Architecture)")
    (license license:expat)))
(define-public rust-alsa-0.3
  (package
    (name "rust-alsa")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "alsa" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ljwyg1ckyglkjf3axhpfs4hspw2pxzr4qcis6w7r7c7ni75wspy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-alsa-sys" ,rust-alsa-sys-0.1)
                       ("rust-bitflags" ,rust-bitflags-0.9)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-nix" ,rust-nix-0.14))))
    (home-page "https://github.com/diwic/alsa-rs")
    (synopsis "Thin but safe wrappers for ALSA (Linux sound API)")
    (description "Thin but safe wrappers for ALSA (Linux sound API)")
    (license (list license:asl2.0 license:expat))))

(define-public rust-libdbus-sys-0.2
  (package
    (name "rust-libdbus-sys")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libdbus-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ss51n616qr36jw34kxvh3m5m6sd7l499xcg7bpj62chmnvvb1f1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/diwic/dbus-rs")
    (synopsis "FFI bindings to libdbus.")
    (description "FFI bindings to libdbus.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-dbus-0.9
  (package
    (name "rust-dbus")
    (version "0.9.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dbus" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1dlf2jzf7sjqz437aj9ksj885nzm5685m72jdb94wp1fdpawv2vg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-executor" ,rust-futures-executor-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libdbus-sys" ,rust-libdbus-sys-0.2)
                       ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/diwic/dbus-rs")
    (synopsis
     "Bindings to D-Bus, which is a bus commonly used on Linux for inter-process communication.")
    (description
     "Bindings to D-Bus, which is a bus commonly used on Linux for inter-process
communication.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-libdbus-sys-0.2
  (package
    (name "rust-libdbus-sys")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libdbus-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ss51n616qr36jw34kxvh3m5m6sd7l499xcg7bpj62chmnvvb1f1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/diwic/dbus-rs")
    (synopsis "FFI bindings to libdbus.")
    (description "FFI bindings to libdbus.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-dbus-0.9
  (package
    (name "rust-dbus")
    (version "0.9.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dbus" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1dlf2jzf7sjqz437aj9ksj885nzm5685m72jdb94wp1fdpawv2vg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-executor" ,rust-futures-executor-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libdbus-sys" ,rust-libdbus-sys-0.2)
                       ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs (("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/diwic/dbus-rs")
    (synopsis
     "Bindings to D-Bus, which is a bus commonly used on Linux for inter-process communication.")
    (description
     "Bindings to D-Bus, which is a bus commonly used on Linux for inter-process
communication.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-libdbus-sys-0.2
  (package
    (name "rust-libdbus-sys")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libdbus-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ss51n616qr36jw34kxvh3m5m6sd7l499xcg7bpj62chmnvvb1f1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/diwic/dbus-rs")
    (synopsis "FFI bindings to libdbus.")
    (description "FFI bindings to libdbus.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-dbus-0.9
  (package
    (name "rust-dbus")
    (version "0.9.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dbus" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1dlf2jzf7sjqz437aj9ksj885nzm5685m72jdb94wp1fdpawv2vg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-executor" ,rust-futures-executor-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-libdbus-sys" ,rust-libdbus-sys-0.2)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/diwic/dbus-rs")
    (synopsis
     "Bindings to D-Bus, which is a bus commonly used on Linux for inter-process communication.")
    (description
     "Bindings to D-Bus, which is a bus commonly used on Linux for inter-process
communication.")
    (license (list license:asl2.0 license:expat))))
(define-public rust-dbus-crossroads-0.5
  (package
    (name "rust-dbus-crossroads")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dbus-crossroads" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0c3f80dychnx1mi8p2rlr7276pywnqyp6ainmzyk6aq1dlli8ham"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-dbus" ,rust-dbus-0.9))))
    (home-page "https://github.com/diwic/dbus-rs/")
    (synopsis "Framework for writing D-Bus method handlers")
    (description "Framework for writing D-Bus method handlers")
    (license (list license:asl2.0 license:expat))))
(define-public rust-dbus-tokio-0.7
  (package
    (name "rust-dbus-tokio")
    (version "0.7.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dbus-tokio" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "10112g227iasjiid7y9wrvnmxypfrczcymj2k5yjvcjk1i5ag88j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-dbus" ,rust-dbus-0.9)
                       ("rust-dbus-crossroads" ,rust-dbus-crossroads-0.5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-tokio" ,rust-tokio-1))
       #:cargo-development-inputs (("rust-dbus-tree" ,rust-dbus-tree-0.9)
                                   ("rust-futures" ,rust-futures-0.3)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/diwic/dbus-rs")
    (synopsis
     "Makes it possible to use Tokio with D-Bus, which is a bus commonly used on Linux for inter-process communication.")
    (description
     "Makes it possible to use Tokio with D-Bus, which is a bus commonly used on Linux
for inter-process communication.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-spotifyd-0.3
  (package
    (name "rust-spotifyd")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "spotifyd" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ppjn9wmqn5rg5931lkw6m4ish1rq37ja3dfxaclq7rmgkb50byl"))))
    (build-system cargo-build-system)
    (native-inputs (list pkg-config))
    (inputs (list alsa-lib openssl))
    (arguments
     `(#:cargo-inputs (("rust-alsa" ,rust-alsa-0.3)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-color-eyre" ,rust-color-eyre-0.5)
                       ("rust-daemonize" ,rust-daemonize-0.4)
                       ("rust-dbus" ,rust-dbus-0.9)
                       ("rust-dbus-tokio" ,rust-dbus-tokio-0.7)
                       ("rust-fern" ,rust-fern-0.6)
                       ("rust-futures" ,rust-futures-0.1)
                       ("rust-gethostname" ,rust-gethostname-0.2)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-keyring" ,rust-keyring-0.10)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-librespot" ,rust-librespot-0.1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-rspotify" ,rust-rspotify-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha-1" ,rust-sha-1-0.9)
                       ("rust-structopt" ,rust-structopt-0.3)
                       ("rust-syslog" ,rust-syslog-4)
                       ("rust-tokio-core" ,rust-tokio-core-0.1)
                       ("rust-tokio-io" ,rust-tokio-io-0.1)
                       ("rust-tokio-signal" ,rust-tokio-signal-0.1)
                       ("rust-toml" ,rust-toml-0.5)
                       ("rust-url" ,rust-url-1)
                       ("rust-whoami" ,rust-whoami-0.9)
                       ("rust-xdg" ,rust-xdg-2))
       #:cargo-development-inputs (("rust-env-logger" ,rust-env-logger-0.7))))
    (home-page "https://github.com/Spotifyd/spotifyd")
    (synopsis "A Spotify daemon")
    (description "This package provides a Spotify daemon")
    (license license:gpl3)))

rust-spotifyd-0.3
