(define-module (magi home packages)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu services syncthing)
  #:use-module (gnu home services)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (rde packages emacs)
  #:use-module (rde home services wm)
  #:use-module (gnu home-services emacs)
;;; for custom packages
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages))

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

(define-public fonts
  (map specification->package
       (list
	"font-fira-code"
	"font-google-roboto"
	"font-awesome")))

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

(define-public utilities
  (map specification->package+output
       (list
	"git:send-email"
	"waypipe"
	"swi-prolog"
	"ripgrep"
	"curl"
	"slirp4netns"
	"vlc"
        "steam"
	"nmap"
       	"network-manager-applet")))

(define-public browsers
  (map specification->package
       (list
	"icecat"
	"firefox")))

(define-public cc-toolchain
  (map specification->package
       (list
	"gcc-toolchain"
	"meson"
	"make"
	"pkg-config"
	"cmake")))

(define-public avr-toolchain
  (map specification->package
       (list
	"avr-toolchain")))

(define-public haskell-toolchain
  (map specification->package
       (list
                                        ;	"gmp"
                                        ;	"ncurses"
                                        ;	"xz"
	"cabal-install"
	"ghc"
	"hlint"
	"hoogle"
	"stylish-haskell"
	"readline"
	)))

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

(define-public emacs-packages
  (append (map specification->package
	       (list
;;; global
		"emacs-use-package"
		"emacs-orderless"
		"emacs-vertico"
		"emacs-consult"
		"emacs-marginalia"
		"emacs-which-key"
                "emacs-ace-jump-mode"
;;; visual
		"emacs-nano-modeline"
					; "emacs-embark"
;;; apps
		"emacs-vterm"
		"emacs-guix"
;;; languages
		"emacs-corfu"
		"emacs-cape"
		"emacs-eglot"
                "emacs-eldoc"
                "emacs-eldoc-box"
		"emacs-tramp"
		"emacs-docker-tramp"
                "emacs-smartparens"
;;; latex
                "emacs-cdlatex"
;;; web
		"emacs-web-mode"
		"emacs-typescript-mode"
;;; zig
                "emacs-zig-mode"
;;; lisp
		"emacs-geiser"
		"emacs-geiser-guile"
                "emacs-geiser-racket"
                "emacs-racket-mode"
		"emacs-paredit"
		"emacs-haskell-mode"
;;; prolog
                "emacs-flymake-swi-prolog"
;;; coq
                "proof-general"
                "emacs-company-coq"
;;; godot
                "emacs-gdscript-mode"
;;; org
		"emacs-org"
                "emacs-org-contrib"
		"emacs-org-roam"
                "emacs-olivetti"
;;; markdown
		"emacs-markdown-mode"
;;; csharp
		"emacs-csharp-mode"
		))
	  (list emacs-nano-agenda emacs-nano-theme)))

(define-public emacs-services
  (list
   (simple-service 'emacs-init
		   home-files-service-type
		   `((".config/emacs/early-init.el" ,(local-file "../../configuration/emacs/early-init.el"))
		     (".config/emacs/init.el" ,(local-file "../../configuration/emacs/init.el"))
                     (".config/emacs/coq-literal.el" ,(local-file "../../configuration/emacs/coq-literal.el"))))
   (service home-emacs-service-type
	    (home-emacs-configuration
	     (package emacs-next-pgtk)
	     (xdg-flavor? #t)
	     (elisp-packages emacs-packages)))))

(define-public syncthing-package
  (specification->package "syncthing"))

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
			(dwt enablet)))
		;;; Outputs
		(set $laptop-monitor "\"eDP-1\"")
		(set $desktop-monitor "\"Dell Inc. DELL U2412M 0FFXD33M4E0S\"")
		(set $tv-monitor "\"Sony SONY TV 0x00000101\"")
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
		(bindsym $mod+Return exec $term)
		(bindsym $mod+Space exec "($menu)")
		(bindsym $mod+e exec emacsclient -c)
		(set $up p)
		(set $down n)
		(set $right f)
		(set $left b)
		;;; Windows
		(bindsym $mod+$up focus up)
		(bindsym $mod+$down focus down)
		(bindsym $mod+$right focus right)
		(bindsym $mod+$left focus left)
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
		(bindsym $mod+Control+Shift+Down  move workspace to output $laptop-monitor)
		;;; Windows
		(bindsym $mod+Up focus container up)
		))))))
;; (service home-waybar-service-type
;; 	    (home-waybar-configuration
;; 	     (config
;; 	      `(()))))

(define-public configuration-services
  (list
   (simple-service 'config-files
		   home-files-service-type
		   `((".config/guile/guile-geiser.scm" ,(local-file "../../configuration/guile/guile-geiser.scm"))
		     (".config/sway/the-great-wawe-off-kanagawa.jpg" ,(local-file "../../configuration/sway/the-great-wawe-off-kanagawa.jpg"))
		     (".config/waybar/config" ,(local-file "../../configuration/waybar/config"))
		     (".config/waybar/style.css" ,(local-file "../../configuration/waybar/style.css"))
		     (".config/alacritty/alacritty.yml" ,(local-file "../../configuration/alacritty/alacritty.yml"))))))
