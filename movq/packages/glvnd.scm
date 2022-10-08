(define-module (movq packages glvnd)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xdisorg))

(define-public libglvnd-guix
  (package/inherit libglvnd
    (name "libglvnd")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/glvnd/libglvnd.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nvlcwzivrdchp70i2l7ic7qdlsdmlsb0ckydscr43rhqldswx69"))
       (patches
        (list (local-file "patches/glvnd-dlopen-path.patch")))))))

(define-public libepoxy-glvnd
  (package/inherit libepoxy
    (name "libepoxy")
    (arguments
     (substitute-keyword-arguments (package-arguments libepoxy)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((python (assoc-ref inputs "python"))
                   (glvnd (assoc-ref inputs "libglvnd")))
               (substitute* "src/gen_dispatch.py"
                 (("/usr/bin/env python") python))
               (substitute* (find-files "." "\\.[ch]$")
                 (("libGL.so.1") (string-append glvnd "/lib/libGL.so.1"))
                 (("libEGL.so.1") (string-append glvnd "/lib/libEGL.so.1")))
               #t)))))))
    (propagated-inputs (list libglvnd))
    (description (string-concatenate (list (package-description libepoxy) " Glvnd-enabled variant.")))))

(define-public libdrm-current
  (package/inherit libdrm
    (name "libdrm")
    (version "2.4.113")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/mesa/drm.git")
             (commit (string-append "libdrm-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rszknx8q4k9j09z926qi8nj6f87x2hjr7p717g3s0mbb5x7lcpx"))))))

(define-public mesa-glvnd
  (package/inherit mesa
    (name "mesa-glvnd")
    (version "22.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/mesa/mesa.git")
             (commit (string-append "mesa-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cmk9qfp2bf8gqrs9bczn32bw9vkh9yvbs2drsv9xmpyp3gaqfcq"))))
    (arguments
     (substitute-keyword-arguments (package-arguments mesa)
       ((#:configure-flags flags)
        `(cons* "-Dglvnd=true"
                "-Dgallium-drivers=iris,swrast,virgl,zink"
                (delete "-Dgallium-drivers=iris,nouveau,r300,r600,radeonsi,svga,swrast,virgl"
                        (delete "-Ddri-drivers=i915,i965,nouveau,r200,r100"
                                (delete "-Dgallium-xa=enabled" ,flags)))))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs mesa)
       (replace "libdrm" libdrm-current)
       (append libglvnd-guix)))
    (inputs
     (modify-inputs (package-inputs mesa)
       (replace "wayland-protocols" wayland-protocols-next)))
    (description (string-concatenate (list (package-description mesa) " Glvnd-enabled variant.")))))

(define-public glvd
  (package
    (inherit libglvnd-guix)
    (name "glvd")
    (version "1.5.00")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build union))
      #:builder #~(begin
                    (use-modules (guix build union)
                                 (srfi srfi-1)
                                 (ice-9 regex))
                    (union-build (assoc-ref %outputs "out")
                                 (list #$mesa-glvnd #$libglvnd-guix)
                                 #:resolve-collision
                                 (lambda (files)
                                   (let ((file
                                          (if (string-match "libglvnd" (first files))
                                              (first files)
                                              (last files))))
                                     (format #t "chosen ~a ~%" file)
                                     file))))))
    (description "libglvnd intended to be grafted in place of mesa.")
    (inputs
     (list mesa-glvnd
           libglvnd-guix))
    (outputs '("out"))))

