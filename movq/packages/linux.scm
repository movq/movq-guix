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
