(define-module (movq packages golang)
  #:use-module (gnu packages golang)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public go-1.18
  (package
    (inherit go-1.17)
    (name "go")
    (version "1.18.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/go")
             (commit (string-append "go" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1s2xwgd3mfbjdf7ls9gyj7n1lbqc4276qkr3znyq9694isj1ak20"))))))
