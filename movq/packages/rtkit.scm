(define-module (movq packages rtkit)
  #:use-module (guix build utils)
  #:use-module (guix build-system meson)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages vim))

(define-public rtkit
  (package
    (name "rtkit")
    (version "0.13")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/heftig/rtkit.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256 (base32 "14z09cxahpwvn229vhg1a7fqp65fdjx6rlis2jlb44i10y9fyjyk"))))
    (build-system meson-build-system)
    (arguments
      (list #:configure-flags
            #~(list (string-append "-Ddbus_interfacedir=" #$output "/share/dbus-1/interfaces")
                    (string-append "-Dpolkit_actiondir=" #$output "/share/polkit-1/actions")
                    (string-append "-Ddbus_systemservicedir=" #$output "/share/dbus-1/system-services"))))
    (native-inputs (list xxd pkg-config))
    (inputs (list polkit dbus libcap))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))

