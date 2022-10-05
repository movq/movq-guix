(define-module (movq packages linux)
  #:use-module (nongnu packages linux)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages python)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1))

(define-public linux-movq-xps13
  (package
    (inherit linux)
    (name "linux-movq-xps13")
    (native-inputs
      `(("kconfig" ,(local-file "files/xps13.config"))
        ("zstd" ,zstd)
       ,@(alist-delete "kconfig"
                       (package-native-inputs linux))))))

(define-public linux-alienware
  (package
    (inherit linux)
    (name "linux-alienware")
    (native-inputs
      `(("kconfig" ,(local-file "files/alienware.config"))
        ("zstd" ,zstd)
        ("zlib" ,zlib)
        ("python" ,python)
       ,@(alist-delete "kconfig"
                       (package-native-inputs linux))))))

(define-public pipewire-current
  (package
    (inherit pipewire-0.3)
    (name "pipewire")
    (version "0.3.56")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/PipeWire/pipewire")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "066g4ri2k8pdflclvr2919f6x98gmqrqyj1xyiingw2nn2pwgcf1"))))))

(define-public wireplumber-current
  (package
    (inherit wireplumber)
    (name "wireplumber")
    (version "0.4.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://gitlab.freedesktop.org/pipewire/wireplumber.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k6ylqq4r1xzc7bbmwy1qgwpqra8shdvilwm1pvzzvilgyqkm8ys"))))
    (inputs (modify-inputs (package-inputs wireplumber)
              (replace "pipewire" pipewire-current)))))
