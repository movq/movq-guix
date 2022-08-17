(define-module (movq packages package-management)
  #:use-module (gnu packages)
  #:use-module (gnu packages package-management)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public flatpak-without-seccomp
  (package
    (inherit flatpak)
    (inputs (modify-inputs (package-inputs flatpak)
              (delete "libseccomp")))
    (arguments
      (substitute-keyword-arguments (package-arguments flatpak)
        ((#:configure-flags flags '())
         `(append ,flags
                  '("--disable-seccomp")))))))
