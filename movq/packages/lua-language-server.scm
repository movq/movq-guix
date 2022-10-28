(define-module (movq packages lua-language-server)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages ninja))

(define-public lua-language-server
  (package
    (name "lua-language-server")
    (version "3.5.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/sumneko/lua-language-server")
               (commit version)
               (recursive? #t)))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "17f0lvn7g54rmb3cm4672cxxyby37kr8iardj6s83zdal0vzykjb"))))
    (build-system gnu-build-system)
    (arguments
      (list #:phases
            #~(modify-phases %standard-phases
                (delete 'configure)
                (delete 'check)
                (add-after 'unpack 'disable-tests
                  (lambda _
                    (substitute* "make.lua"
                      (("\"unit-test\"," all) ""))))
                (replace 'build
                  (lambda _
                    (invoke "ninja" "-C" "3rd/luamake" "-f" "compile/ninja/linux.ninja")
                    (invoke "3rd/luamake/luamake" "rebuild")))
                (replace 'install
                  (lambda _
                    (let* ((libdir (string-append #$output "/lib/" #$name))
                           (bindir (string-append #$output "/bin")))
                      (install-file "bin/lua-language-server" (string-append libdir "/bin"))
                      (install-file "bin/main.lua" (string-append libdir "/bin"))
                      (install-file "main.lua" (string-append libdir))
                      (install-file "debugger.lua" (string-append libdir))
                      (copy-recursively "locale" (string-append libdir "/locale"))
                      (copy-recursively "meta" (string-append libdir "/meta"))
                      (copy-recursively "script" (string-append libdir "/script"))
                      (mkdir (string-append #$output "/bin"))
                      (with-output-to-file (string-append #$output "/bin/lua-language-server")
                        (lambda _
                          (format #t "#!~a/bin/bash
TMPPATH=$(~a/bin/mktemp -d \"/tmp/lua-language-server.XXXX\")
exec ~a/lib/lua-language-server/bin/lua-language-server -E ~a/lib/lua-language-server/main.lua \\
  --logpath=$TMPPATH/log --metapath=$TMPPATH/metapath
" #$bash-minimal #$coreutils-minimal #$output #$output)))
                      (chmod (string-append #$output "/bin/lua-language-server") #o555)))))))
    (native-inputs (list ninja))
    (home-page #f)
    (description #f)
    (synopsis #f)
    (license #f)))
