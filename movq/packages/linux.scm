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
  (let ((base (customize-linux #:name "linux-movq-xps13"
                               #:linux linux
                               #:defconfig (local-file "files/xps13.config"))))
    (package/inherit base
      (native-inputs
        (modify-inputs (package-native-inputs base)
          (append zstd))))))

(define-public linux-alienware
  (let ((base (customize-linux #:name "linux-alienware"
                               #:linux linux
                               #:defconfig (local-file "files/alienware.config"))))
    (package/inherit base
      (native-inputs
        (modify-inputs (package-native-inputs base)
          (append zstd)
          (append zlib)
          (append python))))))
