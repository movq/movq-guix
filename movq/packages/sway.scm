(define-module (movq packages sway)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system meson)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (movq packages glvnd))

(define-public wlroots-current
  (package
    (name "wlroots")
    (version "0.15.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/wlroots/wlroots.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00s73nhi3sc48l426jdlqwpclg41kx1hv0yk4yxhbzw19gqpfm1h"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'hardcode-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "xwayland/server.c"
               (("Xwayland") (string-append (assoc-ref inputs
                                                       "xorg-server-xwayland")
                                            "/bin/Xwayland")))
             #t)))))
    (propagated-inputs
     (list ;; As required by wlroots.pc.
           eudev
           libinput-minimal
           libxkbcommon
           mesa-glvnd
           pixman
           libseat
           wayland
           wayland-protocols
           xcb-util-errors
           xcb-util-wm
           xorg-server-xwayland))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/swaywm/wlroots")
    (synopsis "Pluggable, composable, unopinionated modules for building a
Wayland compositor")
    (description "wlroots is a set of pluggable, composable, unopinionated
modules for building a Wayland compositor.")
    (license license:expat)))

(define-public sway
  (package
    (name "sway")
    (version "1.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/swaywm/sway")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ss3l258blyf2d0lwd7pi7ga1fxfj8pxhag058k7cmjhs3y30y5l"))))
    (build-system meson-build-system)
    (arguments
     `(;; elogind is propagated by wlroots -> libseat
       ;; and would otherwise shadow basu.
       #:configure-flags '("-Dsd-bus-provider=basu")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'hardcode-paths
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Hardcode path to swaybg.
             (substitute* "sway/config.c"
               (("strdup..swaybg..")
                (string-append "strdup(\"" (assoc-ref inputs "swaybg")
                               "/bin/swaybg\")")))
             ;; Hardcode path to scdoc.
             (substitute* "meson.build"
               (("scdoc.get_pkgconfig_variable..scdoc..")
                (string-append "'" (assoc-ref inputs "scdoc")
                               "/bin/scdoc'")))
             #t)))))
    (inputs (list basu
                  cairo
                  gdk-pixbuf
                  json-c
                  libevdev
                  libinput-minimal
                  libxkbcommon
                  pango
                  swaybg
                  wayland
                  wlroots-current))
    (native-inputs
     (list linux-pam mesa-glvnd pkg-config scdoc wayland-protocols))
    (home-page "https://github.com/swaywm/sway")
    (synopsis "Wayland compositor compatible with i3")
    (description "Sway is a i3-compatible Wayland compositor.")
    (license license:expat)))

(define-public swaybg
  (package
    (name "swaybg")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/swaywm/swaybg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dra4xkb4n15npwa3bawcdab96l9l40ffv3hack3f2myyagy3prf"))))
    (build-system meson-build-system)
    (inputs (list cairo gdk-pixbuf wayland))
    (native-inputs (list pkg-config scdoc wayland-protocols))
    (home-page "https://github.com/swaywm/sway")
    (synopsis "Screen wallpaper utility for Wayland compositors")
    (description "Swaybg is a wallpaper utility for Wayland compositors.")
    (license license:expat))) ; MIT license
