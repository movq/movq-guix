(define-module (movq packages linux)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

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
    (version "0.4.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://gitlab.freedesktop.org/pipewire/wireplumber.git")
             (commit version)))
       (file-name (git-file-name name version))
       (patches (list (local-file "patches/wireplumber-cpu-fix.patch")))
       (sha256
        (base32 "09pg5cki1xn9bwn3bcjdc54z7b4iqkk1dhn560qyjcglq8xg7nnw"))))
    (inputs (modify-inputs (package-inputs wireplumber)
              (replace "pipewire" pipewire-current)))))
