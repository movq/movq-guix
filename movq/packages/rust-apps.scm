(define-module (movq packages rust-apps)
  #:use-module (guix build utils)
  #:use-module (guix build-system cargo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages rust)
  #:use-module (movq packages crates-io)
  #:use-module (movq packages rust))

(define-public rust-analyzer
  (package
    (name "rust-analyzer")
    (version "2022-10-24")
    (source
     (origin
       ;; The crate at "crates.io" is empty.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rust-lang/rust-analyzer.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0h1skd5z3245yby2bwaj2k2s9wvff6dsg1n4g4wpz9vfdld158dc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:rust ,rust-current
       #:tests? #f
       #:install-source? #f             ; virtual manifest
       #:cargo-test-flags
       '("--release" "--"
         "--skip=tests::test_version_check" ;it need rustc's version
         ;; FIXME: Guix's rust does not install source in
         ;; %out/lib/rustlib/src/rust so "can't load standard library from
         ;; sysroot"
         "--skip=tests::test_loading_rust_analyzer"
         ;; Failed to run rustfmt from toolchain 'stable'.  Please run `rustup
         ;; component add rustfmt --toolchain stable` to install it
         "--skip=tests::sourcegen::sourcegen_assists_docs" ;need rustfmt
         "--skip=tests::sourcegen_ast::sourcegen_ast"      ;same

         "--skip=tidy::cargo_files_are_tidy"    ;not needed
         "--skip=tidy::check_licenses"          ;it runs cargo metadata
         "--skip=tidy::check_merge_commits"     ;it runs git rev-list
         "--skip=tidy::check_code_formatting"   ;need rustfmt as cargo fmt
         "--skip=tidy::generate_grammar"        ;same
         "--skip=tidy::generate_assists_tests") ;same
       #:cargo-development-inputs
       (("rust-arbitrary" ,rust-arbitrary-1)
        ("rust-derive-arbitrary" ,rust-derive-arbitrary-1)
        ("rust-expect-test" ,rust-expect-test-1)
        ("rust-oorandom" ,rust-oorandom-11.1)
        ("rust-quote" ,rust-quote-1)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
        ("rust-tracing-tree" ,rust-tracing-tree-0.2)
        ("rust-ungrammar" ,rust-ungrammar-1))
       #:cargo-inputs
       (("rust-always-assert" ,rust-always-assert-0.1)
        ("rust-anyhow" ,rust-anyhow-1)
        ("rust-anymap" ,rust-anymap-1)
        ("rust-arrayvec" ,rust-arrayvec-0.7)
        ("rust-backtrace" ,rust-backtrace-0.3)
        ("rust-cargo-metadata" ,rust-cargo-metadata-0.15)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-chalk-recursive" ,rust-chalk-recursive-0.86)
        ("rust-chalk-solve" ,rust-chalk-solve-0.86)
        ("rust-countme" ,rust-countme-3)
        ("rust-cov-mark" ,rust-cov-mark-2)
        ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
        ("rust-dashmap" ,rust-dashmap-4)
        ("rust-dissimilar" ,rust-dissimilar-1)
        ("rust-dot" ,rust-dot-0.1)
        ("rust-drop-bomb" ,rust-drop-bomb-0.1)
        ("rust-either" ,rust-either-1)
        ("rust-ena" ,rust-ena-0.14)
        ("rust-env-logger" ,rust-env-logger-0.8)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-fst" ,rust-fst-0.4)
        ("rust-home" ,rust-home-0.5)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-itertools" ,rust-itertools-0.10)
        ("rust-jod-thread" ,rust-jod-thread-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libloading" ,rust-libloading-0.7)
        ("rust-log" ,rust-log-0.4)
        ("rust-lsp-server" ,rust-lsp-server-0.5)
        ("rust-lsp-types" ,rust-lsp-types-0.93)
        ("rust-memmap2" ,rust-memmap2-0.5)
        ("rust-mimalloc" ,rust-mimalloc-0.1)
        ("rust-miow" ,rust-miow-0.4)
        ("rust-notify" ,rust-notify-5)
        ("rust-object" ,rust-object-0.28)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-perf-event" ,rust-perf-event-0.4)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-pulldown-cmark" ,rust-pulldown-cmark-0.9)
        ("rust-pulldown-cmark-to-cmark" ,rust-pulldown-cmark-to-cmark-10)
        ("rust-rowan" ,rust-rowan-0.15)
        ("rust-rustc-ap-rustc-lexer" ,rust-rustc-ap-rustc-lexer-725)
        ("rust-rustc-hash" ,rust-rustc-hash-1)
        ("rust-salsa" ,rust-salsa-0.17)
        ("rust-scoped-tls" ,rust-scoped-tls-1)
        ("rust-scip" ,rust-scip-0.1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-path-to-error" ,rust-serde-path-to-error-0.1)
        ("rust-typed-arena" ,rust-typed-arena-2)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-smol-str" ,rust-smol-str-0.1)
        ("rust-snap" ,rust-snap-1)
        ("rust-text-size" ,rust-text-size-1)
        ("rust-threadpool" ,rust-threadpool-1)
        ("rust-tikv-jemalloc-ctl" ,rust-tikv-jemalloc-ctl-0.5)
        ("rust-tikv-jemallocator" ,rust-tikv-jemallocator-0.5)
        ("rust-url" ,rust-url-2)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-write-json" ,rust-write-json-0.1)
        ("rust-xflags" ,rust-xflags-0.3)
        ("rust-xshell" ,rust-xshell-0.2))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-tests
           (lambda _
             (let ((bash (string-append "#!" (which "bash"))))
               (with-directory-excursion "crates/parser/test_data/lexer/ok"
                 (substitute* "single_line_comments.txt"
                   (("SHEBANG 19")
                    (string-append "SHEBANG "
                                   (number->string (string-length bash))))
                   (("#!/usr/bin/env bash") bash))))))
         (add-before 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/rust-analyzer-"
                                        ,version)))
               (copy-recursively "docs" doc))))
         (add-before 'install 'chdir
           (lambda _
             (chdir "crates/rust-analyzer")))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (rust-src-path (search-input-directory
                                    inputs "/lib/rustlib/src/rust/library")))
               ;; if environment variable RUST_SRC_PATH is not set, set it,
               ;; make rust-analyzer work out of box.
               (with-directory-excursion bin
                 (let* ((prog "rust-analyzer")
                        (wrapped-file (string-append (dirname prog)
                                                     "/." (basename prog) "-real"))
                        (prog-tmp (string-append wrapped-file "-tmp")))
                   (link prog wrapped-file)
                   (call-with-output-file prog-tmp
                     (lambda (port)
                       (format port "#!~a
if test -z \"${RUST_SRC_PATH}\";then export RUST_SRC_PATH=~S;fi;
exec -a \"$0\" \"~a\" \"$@\""
                               (which "bash")
                               rust-src-path
                               (canonicalize-path wrapped-file))))
                   (chmod prog-tmp #o755)
                   (rename-file prog-tmp prog))))))
         (replace 'install-license-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/rust-analyzer-"
                                        ,version)))
               (chdir "../..")
               (install-file "LICENSE-MIT" doc)
               (install-file "LICENSE-APACHE" doc)))))))
    (native-inputs (list rust-current-src))
    (home-page "https://rust-analyzer.github.io/")
    (synopsis "Experimental Rust compiler front-end for IDEs")
    (description "Rust-analyzer is a modular compiler frontend for the Rust
language.  It is a part of a larger rls-2.0 effort to create excellent IDE
support for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-libz-sys-1
  (package
    (name "rust-libz-sys")
    (version "1.1.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libz-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1gqb8nk7j4ngvlcll8plm2fvjwic40p2g4qp20pwry1m74f7c0lp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-cmake" ,rust-cmake-0.1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-vcpkg" ,rust-vcpkg-0.2))))
    (home-page "https://github.com/rust-lang/libz-sys")
    (synopsis
     "Low-level bindings to the system libz library (also known as zlib).")
    (description
     "Low-level bindings to the system libz library (also known as zlib).")
    (license (list license:expat license:asl2.0))))

(define-public rust-libz-ng-sys-1
  (package
    (name "rust-libz-ng-sys")
    (version "1.1.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libz-ng-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1wp0aya5hh76h1acspvrrsvq2fl0kyb8dpi6wy0zaswnm6bax6a3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cmake" ,rust-cmake-0.1)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/rust-lang/libz-sys")
    (synopsis
     "Low-level bindings to zlib-ng (libz-ng), a high-performance zlib library.")
    (description
     "Low-level bindings to zlib-ng (libz-ng), a high-performance zlib library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-flate2-1
  (package
    (name "rust-flate2")
    (version "1.0.24")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "flate2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xmzzg91c0hdl39qz0hwph0w629bva1dh21j3zyqp7xd4x60yazq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cloudflare-zlib-sys" ,rust-cloudflare-zlib-sys-0.3)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-libz-ng-sys" ,rust-libz-ng-sys-1)
                       ("rust-libz-sys" ,rust-libz-sys-1)
                       ("rust-miniz-oxide" ,rust-miniz-oxide-0.5))
       #:cargo-development-inputs (("rust-quickcheck" ,rust-quickcheck-1)
                                   ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/rust-lang/flate2-rs")
    (synopsis
     "DEFLATE compression and decompression exposed as Read/BufRead/Write streams.
Supports miniz_oxide and multiple zlib implementations. Supports zlib, gzip,
and raw deflate streams.
")
    (description
     "DEFLATE compression and decompression exposed as Read/BufRead/Write streams.
Supports miniz_oxide and multiple zlib implementations.  Supports zlib, gzip,
and raw deflate streams.")
    (license (list license:expat license:asl2.0))))

(define-public rust-xflags-macros-0.3
  (package
    (name "rust-xflags-macros")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "xflags-macros" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1a6123c3qy1sqjsvlc357b2w8vr161vnlyxqwsm96w4pm0y7p3pm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/matklad/xflags")
    (synopsis "Private implementation details of xflags.")
    (description "Private implementation details of xflags.")
    (license (list license:expat license:asl2.0))))

(define-public rust-xflags-0.3
  (package
    (name "rust-xflags")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "xflags" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1sn2y82vsjvmjd4j9rxg3pb28ig3dj7nphb9hciwml120mc4nmf4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-xflags-macros" ,rust-xflags-macros-0.3))))
    (home-page "https://github.com/matklad/xflags")
    (synopsis "Moderately simple command line arguments parser.")
    (description "Moderately simple command line arguments parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-xshell-macros-0.2
  (package
    (name "rust-xshell-macros")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "xshell-macros" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "06n406clanafsg2zppk78xvggwynha7m6n6q8dfbznbdq9b1nc48"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/matklad/xshell")
    (synopsis "Private implementation detail of xshell crate")
    (description "Private implementation detail of xshell crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-xshell-0.2
  (package
    (name "rust-xshell")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "xshell" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "10d9xi5751p3bjpb8dfxxwxrplfn5m1b6l8qwjqk8ln8qmyhjivd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-xshell-macros" ,rust-xshell-macros-0.2))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1))))
    (home-page "https://github.com/matklad/xshell")
    (synopsis "Utilities for quick shell scripting in Rust")
    (description "Utilities for quick shell scripting in Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-lsp-types-0.93
  (package
    (name "rust-lsp-types")
    (version "0.93.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "lsp-types" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0d374h95ggz69f24ind7fpgxfpvn105m9nvyi2x8bryx2pizxg53"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-repr" ,rust-serde-repr-0.1)
                       ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/gluon-lang/lsp-types")
    (synopsis
     "Types for interaction with a language server, using VSCode's Language Server Protocol")
    (description
     "Types for interaction with a language server, using VSCode's Language Server
Protocol")
    (license license:expat)))

(define-public rust-arbitrary-1
  (package
    (name "rust-arbitrary")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "arbitrary" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "106qgz0qxs202xlvjfyvw8dkb6ynr1ymmcclfh89l56mj2zpzm19"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-derive-arbitrary" ,rust-derive-arbitrary-1))))
    (home-page "https://github.com/rust-fuzz/arbitrary/")
    (synopsis
     "The trait for generating structured data from unstructured data")
    (description
     "The trait for generating structured data from unstructured data")
    (license (list license:expat license:asl2.0))))

(define-public rust-expect-test-1
  (package
    (name "rust-expect-test")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "expect-test" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1lsd6vhy4f7wggdh1k60winn28424gj2iq9gqyvnx0ldlfn62ihx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-dissimilar" ,rust-dissimilar-1)
                       ("rust-once-cell" ,rust-once-cell-1))))
    (home-page "https://github.com/rust-analyzer/expect-test")
    (synopsis "Minimalistic snapshot testing library")
    (description "Minimalistic snapshot testing library")
    (license (list license:expat license:asl2.0))))

(define-public rust-smol-str-0.1
  (package
    (name "rust-smol-str")
    (version "0.1.23")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "smol-str" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0i5b6mm2hbmvyvch3rhfx6bfl9jmijx320ffazhs5qxp52512xbl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-proptest" ,rust-proptest-1)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/rust-analyzer/smol_str")
    (synopsis "small-string optimized string type with O(1) clone")
    (description "small-string optimized string type with O(1) clone")
    (license (list license:expat license:asl2.0))))

(define-public rust-smallvec-1
  (package
    (name "rust-smallvec")
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "smallvec" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1q2k15fzxgwjpcdv3f323w24rbbfyv711ayz85ila12lg7zbw1x5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                   ("rust-debugger-test" ,rust-debugger-test-0.1)
                                   ("rust-debugger-test-parser" ,rust-debugger-test-parser-0.1))))
    (home-page "https://github.com/servo/rust-smallvec")
    (synopsis
     "'Small vector' optimization: store up to a small number of items on the stack")
    (description
     "Small vector optimization: store up to a small number of items on the stack")
    (license (list license:expat license:asl2.0))))

(define-public rust-itertools-0.10
  (package
    (name "rust-itertools")
    (version "0.10.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "itertools" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ww45h7nxx5kj6z2y6chlskxd1igvs4j507anr6dzg99x1h25zdh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-either" ,rust-either-1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-paste" ,rust-paste-1)
                                   ("rust-permutohedron" ,rust-permutohedron-0.2)
                                   ("rust-quickcheck" ,rust-quickcheck-0.9)
                                   ("rust-rand" ,rust-rand-0.7))))
    (home-page "https://github.com/rust-itertools/itertools")
    (synopsis
     "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (description
     "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rowan-0.15
  (package
    (name "rust-rowan")
    (version "0.15.10")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rowan" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0cpkf6m93kkwhhy5459x3w80mms01nqym34cwhzr07m3gdz584aq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-countme" ,rust-countme-3)
                       ("rust-hashbrown" ,rust-hashbrown-0.12)
                       ("rust-memoffset" ,rust-memoffset-0.6)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-text-size" ,rust-text-size-1))
       #:cargo-development-inputs (("rust-m-lexer" ,rust-m-lexer-0.0.4))))
    (home-page "https://github.com/rust-analyzer/rowan")
    (synopsis "Library for generic lossless syntax trees")
    (description "Library for generic lossless syntax trees")
    (license (list license:expat license:asl2.0))))

(define-public rust-proc-macro2-1
  (package
    (name "rust-proc-macro2")
    (version "1.0.47")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "proc-macro2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "09g7alc7mlbycsadfh7lwskr1qfxbiic9qp9z751cqz3n04dk8sy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-unicode-ident" ,rust-unicode-ident-1))
       #:cargo-development-inputs (("rust-quote" ,rust-quote-1))))
    (home-page "https://github.com/dtolnay/proc-macro2")
    (synopsis
     "A substitute implementation of the compiler's `proc_macro` API to decouple token-based libraries from the procedural macro use case.")
    (description
     "This package provides a substitute implementation of the compiler's `proc_macro`
API to decouple token-based libraries from the procedural macro use case.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rayon-core-1
  (package
    (name "rust-rayon-core")
    (version "1.9.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rayon-core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gv8k6612gc24kqqm4440f5qfx6gnyv2v6dj3d4libbdmjswv2r5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-crossbeam-deque" ,rust-crossbeam-deque-0.8)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-num-cpus" ,rust-num-cpus-1))))
    (home-page "https://github.com/rayon-rs/rayon")
    (synopsis "Core APIs for Rayon")
    (description "Core APIs for Rayon")
    (license (list license:expat license:asl2.0))))

(define-public rust-rayon-1
  (package
    (name "rust-rayon")
    (version "1.5.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rayon" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0z9sjcy1hnnvgkwx3cn1x44pf24jpwarp3172m9am2xd5rvyb6dx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-crossbeam-deque" ,rust-crossbeam-deque-0.8)
                       ("rust-either" ,rust-either-1)
                       ("rust-rayon-core" ,rust-rayon-core-1))
       #:cargo-development-inputs (("rust-docopt" ,rust-docopt-1)
                                   ("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-rand-xorshift" ,rust-rand-xorshift-0.3)
                                   ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/rayon-rs/rayon")
    (synopsis "Simple work-stealing parallelism for Rust")
    (description "Simple work-stealing parallelism for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-ungrammar-1
  (package
    (name "rust-ungrammar")
    (version "1.16.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ungrammar" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "13ynrv658ikr4lqi3lk1xbcrr1d1qsjnrb8acwfyrwqbgwsdzrd3"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/matklad/ungrammar")
    (synopsis "A DSL for describing concrete syntax trees")
    (description
     "This package provides a DSL for describing concrete syntax trees")
    (license (list license:expat license:asl2.0))))

(define-public rust-dashmap-5
  (package
    (name "rust-dashmap")
    (version "5.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dashmap" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1p2f5kr5hy7h4w3y2abprnxsaq36r685zfx1s8v0nfw2vbgpcw4h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-hashbrown" ,rust-hashbrown-0.12)
                       ("rust-lock-api" ,rust-lock-api-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot-core" ,rust-parking-lot-core-0.9)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/xacrimon/dashmap")
    (synopsis "Blazing fast concurrent HashMap for Rust.")
    (description "Blazing fast concurrent HashMap for Rust.")
    (license license:expat)))

(define-public rust-countme-3
  (package
    (name "rust-countme")
    (version "3.0.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "countme" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0dn62hhvgmwyxslh14r4nlbvz8h50cp5mnn1qhqsw63vs7yva13p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-dashmap" ,rust-dashmap-5)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-rustc-hash" ,rust-rustc-hash-1))))
    (home-page "https://github.com/matklad/countme")
    (synopsis "Counts the number of live instances of types")
    (description "Counts the number of live instances of types")
    (license (list license:expat license:asl2.0))))

(define-public rust-tikv-jemallocator-0.5
  (package
    (name "rust-tikv-jemallocator")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tikv-jemallocator" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0yb9sw5jr382x1jnbxj5d7hng8w585lm6ff8gvahcv1sl6w2sq90"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-tikv-jemalloc-sys" ,rust-tikv-jemalloc-sys-0.5))))
    (home-page "https://github.com/tikv/jemallocator")
    (synopsis "A Rust allocator backed by jemalloc
")
    (description "This package provides a Rust allocator backed by jemalloc")
    (license (list license:expat license:asl2.0))))

(define-public rust-tikv-jemalloc-sys-0.5
  (package
    (name "rust-tikv-jemalloc-sys")
    (version "0.5.2+5.3.0-patched")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tikv-jemalloc-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1hwj4npykla3fsfw3y22l5pij7qqbi6kx21mg1f95l4pm56w2igc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-fs-extra" ,rust-fs-extra-1)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/tikv/jemallocator")
    (synopsis "Rust FFI bindings to jemalloc
")
    (description "Rust FFI bindings to jemalloc")
    (license (list license:expat license:asl2.0))))

(define-public rust-tikv-jemalloc-ctl-0.5
  (package
    (name "rust-tikv-jemalloc-ctl")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tikv-jemalloc-ctl" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1h8n88mg2p7h288mj61i7bz2d72fh10f0ih1gbzis5ab5xbhcxz3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-tikv-jemalloc-sys" ,rust-tikv-jemalloc-sys-0.5))
       #:cargo-development-inputs (("rust-tikv-jemallocator" ,rust-tikv-jemallocator-0.5))))
    (home-page "https://github.com/tikv/jemallocator")
    (synopsis "A safe wrapper over jemalloc's control and introspection APIs
")
    (description
     "This package provides a safe wrapper over jemalloc's control and introspection
APIs")
    (license (list license:expat license:asl2.0))))

(define-public rust-dissimilar-1
  (package
    (name "rust-dissimilar")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dissimilar" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "19flq7cv0y3zdbl4dhjg11q9gxmn8wxdv7383s74pn416livk5wc"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dtolnay/dissimilar")
    (synopsis
     "Diff library with semantic cleanup, based on Google's diff-match-patch")
    (description
     "Diff library with semantic cleanup, based on Google's diff-match-patch")
    (license license:asl2.0)))

(define-public rust-cargo-metadata-0.15
  (package
    (name "rust-cargo-metadata")
    (version "0.15.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cargo-metadata" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qh0xsal6z0hv64w58ilyx4zsfqzhm8ns55k8bvz8s6man98av20"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-camino" ,rust-camino-1)
                       ("rust-cargo-platform" ,rust-cargo-platform-0.1)
                       ("rust-derive-builder" ,rust-derive-builder-0.11)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/oli-obk/cargo_metadata")
    (synopsis "structured access to the output of `cargo metadata`")
    (description "structured access to the output of `cargo metadata`")
    (license license:expat)))

(define-public rust-criterion-0.3
  (package
    (name "rust-criterion")
    (version "0.3.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "criterion" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "13yd64ah93gkbdv7qq4cr6rhgl9979jjcjk3gkhnav1b7glns7dh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-atty" ,rust-atty-0.2)
                       ("rust-cast" ,rust-cast-0.3)
                       ("rust-clap" ,rust-clap-2)
                       ("rust-criterion-plot" ,rust-criterion-plot-0.4)
                       ("rust-csv" ,rust-csv-1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-itertools" ,rust-itertools-0.10)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-oorandom" ,rust-oorandom-11.1)
                       ("rust-plotters" ,rust-plotters-0.3)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-cbor" ,rust-serde-cbor-0.11)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-smol" ,rust-smol-1)
                       ("rust-tinytemplate" ,rust-tinytemplate-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://bheisler.github.io/criterion.rs/book/index.html")
    (synopsis "Statistics-driven micro-benchmarking library")
    (description "Statistics-driven micro-benchmarking library")
    (license (list license:asl2.0 license:expat))))

(define-public rust-tracing-attributes-0.1
  (package
    (name "rust-tracing-attributes")
    (version "0.1.23")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tracing-attributes" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "06h80cy0i1kilvnj8j9dw2kcfwbwj49n2s3jwskhr1rra7sgh5s0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://tokio.rs")
    (synopsis
     "Procedural macro attributes for automatically instrumenting functions.
")
    (description
     "Procedural macro attributes for automatically instrumenting functions.")
    (license license:expat)))

(define-public rust-tracing-0.1
  (package
    (name "rust-tracing")
    (version "0.1.37")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tracing" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1f2fylc79xmbh7v53kak6qyw27njbx227rd64kb4bga8ilxc7s4c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-tracing-attributes" ,rust-tracing-attributes-0.1)
                       ("rust-tracing-core" ,rust-tracing-core-0.1))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-log" ,rust-log-0.4)
                                   ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3))))
    (home-page "https://tokio.rs")
    (synopsis "Application-level tracing for Rust.
")
    (description "Application-level tracing for Rust.")
    (license license:expat)))

(define-public rust-home-0.5
  (package
    (name "rust-home")
    (version "0.5.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "home" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "023liawrnw7w447cd26jjx9mx6zf0gp2lpxjn1bnvh20njs0jwvl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/brson/home")
    (synopsis "Shared definitions of home directories")
    (description "Shared definitions of home directories")
    (license (list license:expat license:asl2.0))))

(define-public rust-either-1
  (package
    (name "rust-either")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "either" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "15z70yaivlkpx27vzv99ibf8d2x5jp24yn69y0xi20w86v4c3rch"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/bluss/either")
    (synopsis
     "The enum `Either` with variants `Left` and `Right` is a general purpose sum type with two cases.
")
    (description
     "The enum `Either` with variants `Left` and `Right` is a general purpose sum type
with two cases.")
    (license (list license:expat license:asl2.0))))

(define-public rust-anymap-1
  (package
    (name "rust-anymap")
    (version "1.0.0-beta.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "anymap" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0whxr4r34rzvw5xwr80clamb4szra0x5kmwp6ygdhl1xdxd8y7wg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-hashbrown" ,rust-hashbrown-0.12))))
    (home-page "https://github.com/chris-morgan/anymap")
    (synopsis "A safe and convenient store for one value of each type")
    (description
     "This package provides a safe and convenient store for one value of each type")
    (license (list license:expat license:asl2.0))))

(define-public rust-chalk-derive-0.86
  (package
    (name "rust-chalk-derive")
    (version "0.86.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "chalk-derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "00klsbkdm2qi6324hq6b1vgd6fl9859q2958cr1hkdamv0ax96al"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-synstructure" ,rust-synstructure-0.12))))
    (home-page "https://github.com/rust-lang/chalk")
    (synopsis "A helper crate for use by chalk crates for `derive` macros.")
    (description
     "This package provides a helper crate for use by chalk crates for `derive`
macros.")
    (license (list license:expat license:asl2.0))))

(define-public rust-chalk-ir-0.86
  (package
    (name "rust-chalk-ir")
    (version "0.86.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "chalk-ir" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1qb0p5xcch30dq4r7i03s9wc9d99lkrh2fnfxsq0g1d4fs61201q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-chalk-derive" ,rust-chalk-derive-0.86)
                       ("rust-lazy-static" ,rust-lazy-static-1))))
    (home-page "https://github.com/rust-lang/chalk")
    (synopsis "Chalk's internal representation of types, goals, and clauses")
    (description
     "Chalk's internal representation of types, goals, and clauses")
    (license (list license:expat license:asl2.0))))

(define-public rust-chalk-solve-0.86
  (package
    (name "rust-chalk-solve")
    (version "0.86.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "chalk-solve" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1181v2sz15m8zl81zz0hk05gjp0axya1p2p591iphn4kiw33r70f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-chalk-derive" ,rust-chalk-derive-0.86)
                       ("rust-chalk-ir" ,rust-chalk-ir-0.86)
                       ("rust-ena" ,rust-ena-0.14)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-itertools" ,rust-itertools-0.10)
                       ("rust-petgraph" ,rust-petgraph-0.5)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
                       ("rust-tracing-tree" ,rust-tracing-tree-0.2))))
    (home-page "https://github.com/rust-lang/chalk")
    (synopsis "Combines the chalk-engine with chalk-ir")
    (description "Combines the chalk-engine with chalk-ir")
    (license (list license:expat license:asl2.0))))

(define-public rust-chalk-recursive-0.86
  (package
    (name "rust-chalk-recursive")
    (version "0.86.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "chalk-recursive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1j5q6pciyjv7c8pannj3hhg7zws85njqc9anz38h8cfpixi61bqv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-chalk-derive" ,rust-chalk-derive-0.86)
                       ("rust-chalk-ir" ,rust-chalk-ir-0.86)
                       ("rust-chalk-solve" ,rust-chalk-solve-0.86)
                       ("rust-rustc-hash" ,rust-rustc-hash-1)
                       ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/rust-lang/chalk")
    (synopsis "Recursive solver for the Chalk project")
    (description "Recursive solver for the Chalk project")
    (license (list license:expat license:asl2.0))))

(define-public rust-tracing-tree-0.2
  (package
    (name "rust-tracing-tree")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tracing-tree" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0lxl1zg5hzbpjwj7l3m4i9j140l29rbqhfc26bjas8f656rr0znh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-ansi-term" ,rust-ansi-term-0.12)
                       ("rust-atty" ,rust-atty-0.2)
                       ("rust-tracing-core" ,rust-tracing-core-0.1)
                       ("rust-tracing-log" ,rust-tracing-log-0.1)
                       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3))
       #:cargo-development-inputs (("rust-assert-cmd" ,rust-assert-cmd-1)
                                   ("rust-glob" ,rust-glob-0.3)
                                   ("rust-log" ,rust-log-0.4)
                                   ("rust-tracing" ,rust-tracing-0.1))))
    (home-page "https://github.com/davidbarsky/tracing-tree")
    (synopsis "A Tracing Layer which prints a tree of spans and events.")
    (description
     "This package provides a Tracing Layer which prints a tree of spans and events.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pulldown-cmark-0.9
  (package
    (name "rust-pulldown-cmark")
    (version "0.9.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pulldown-cmark" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qzd01nqrh0nhfl06brbrnavmp6izj7riznbnmbq2xkqphscd71d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-getopts" ,rust-getopts-0.2)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-unicase" ,rust-unicase-2))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                   ("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-html5ever" ,rust-html5ever-0.25)
                                   ("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-markup5ever-rcdom" ,rust-markup5ever-rcdom-0.1)
                                   ("rust-regex" ,rust-regex-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-tendril" ,rust-tendril-0.4))))
    (home-page "https://github.com/raphlinus/pulldown-cmark")
    (synopsis "A pull parser for CommonMark")
    (description "This package provides a pull parser for CommonMark")
    (license license:expat)))

(define-public rust-pulldown-cmark-to-cmark-10
  (package
    (name "rust-pulldown-cmark-to-cmark")
    (version "10.0.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pulldown-cmark-to-cmark" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gc366cmd5jxal9m95l17rvqsm4dn62lywc8v5gwq8vcjvhyd501"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-pulldown-cmark" ,rust-pulldown-cmark-0.9))
       #:cargo-development-inputs (("rust-indoc" ,rust-indoc-1)
                                   ("rust-pretty-assertions" ,rust-pretty-assertions-0.7))))
    (home-page "https://github.com/Byron/pulldown-cmark-to-cmark")
    (synopsis
     "Convert pulldown-cmark Events back to the string they were parsed from")
    (description
     "Convert pulldown-cmark Events back to the string they were parsed from")
    (license license:asl2.0)))

(define-public rust-libloading-0.7
  (package
    (name "rust-libloading")
    (version "0.7.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libloading" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pfdmf0scimadifda0wyg9swalr2pahwd5fjmvvfjxd7z41hzg7g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs (("rust-libc" ,rust-libc-0.2)
                                   ("rust-static-assertions" ,rust-static-assertions-1))))
    (home-page "https://github.com/nagisa/rust_libloading/")
    (synopsis
     "Bindings around the platform's dynamic library loading primitives with greatly improved memory safety.")
    (description
     "Bindings around the platform's dynamic library loading primitives with greatly
improved memory safety.")
    (license license:isc)))

(define-public rust-libmimalloc-sys-0.1
  (package
    (name "rust-libmimalloc-sys")
    (version "0.1.26")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libmimalloc-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0j7bi0sq3jgqfyzgqp1qxz3jkgj2jnfappqvmaizs2wv52mr7h4g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-cty" ,rust-cty-0.2))))
    (home-page
     "https://github.com/purpleprotocol/mimalloc_rust/tree/master/libmimalloc-sys")
    (synopsis "Sys crate wrapping the mimalloc allocator")
    (description "Sys crate wrapping the mimalloc allocator")
    (license license:expat)))

(define-public rust-mimalloc-0.1
  (package
    (name "rust-mimalloc")
    (version "0.1.30")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "mimalloc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0dfvbjkldka04q48cn4pkwphg1bsffh1r2797kmzkgyk815nmkkn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libmimalloc-sys" ,rust-libmimalloc-sys-0.1))))
    (home-page "https://github.com/purpleprotocol/mimalloc_rust")
    (synopsis "Performance and security oriented drop-in allocator")
    (description "Performance and security oriented drop-in allocator")
    (license license:expat)))

(define-public rust-protobuf-support-3
  (package
    (name "rust-protobuf-support")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "protobuf-support" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "077f13xizb2dh89a23nprnbks6zlgpi3bxzj2piy4zpw2bz5g8cc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/stepancheg/rust-protobuf/")
    (synopsis
     "Code supporting protobuf implementation. None of code in this crate is public API.
")
    (description
     "Code supporting protobuf implementation.  None of code in this crate is public
API.")
    (license license:expat)))

(define-public rust-protobuf-3
  (package
    (name "rust-protobuf")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "protobuf" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xq2af8fnjh4zsc4xj5dw5l76q1620d8s9m6czqwh00qp7cagr2f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-protobuf-support" ,rust-protobuf-support-3)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/stepancheg/rust-protobuf/")
    (synopsis "Rust implementation of Google protocol buffers
")
    (description "Rust implementation of Google protocol buffers")
    (license license:expat)))

(define-public rust-scip-0.1
  (package
    (name "rust-scip")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "scip" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0sqk96c9y0lz8w6gwnc3g1bzk6w3nw211dwdqzbzlsbg508bpgxj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-protobuf" ,rust-protobuf-3))
       #:cargo-development-inputs (("rust-pretty-assertions" ,rust-pretty-assertions-1))))
    (home-page "")
    (synopsis
     "SCIP (pronunciation: \"skip\") is a language-agnostic protocol for indexing source code, which can be used to power code navigation functionality such as Go to definition, Find references, and Find implementations.
")
    (description
     "SCIP (pronunciation: \"skip\") is a language-agnostic protocol for indexing source
code, which can be used to power code navigation functionality such as Go to
definition, Find references, and Find implementations.")
    (license license:asl2.0)))

(define-public rust-threadpool-1
  (package
    (name "rust-threadpool")
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "threadpool" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1amgfyzvynbm8pacniivzq9r0fh3chhs7kijic81j76l6c5ycl6h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-num-cpus" ,rust-num-cpus-1))))
    (home-page "https://github.com/rust-threadpool/rust-threadpool")
    (synopsis
     "A thread pool for running a number of jobs on a fixed set of worker threads.
")
    (description
     "This package provides a thread pool for running a number of jobs on a fixed set
of worker threads.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tikv-jemalloc-sys-0.5
  (package
    (name "rust-tikv-jemalloc-sys")
    (version "0.5.2+5.3.0-patched")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tikv-jemalloc-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1hwj4npykla3fsfw3y22l5pij7qqbi6kx21mg1f95l4pm56w2igc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-fs-extra" ,rust-fs-extra-1)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/tikv/jemallocator")
    (synopsis "Rust FFI bindings to jemalloc
")
    (description "Rust FFI bindings to jemalloc")
    (license (list license:expat license:asl2.0))))

(define-public rust-tikv-jemallocator-0.5
  (package
    (name "rust-tikv-jemallocator")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tikv-jemallocator" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0yb9sw5jr382x1jnbxj5d7hng8w585lm6ff8gvahcv1sl6w2sq90"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-tikv-jemalloc-sys" ,rust-tikv-jemalloc-sys-0.5))
       #:cargo-development-inputs (("rust-paste" ,rust-paste-1))))
    (home-page "https://github.com/tikv/jemallocator")
    (synopsis "A Rust allocator backed by jemalloc
")
    (description "This package provides a Rust allocator backed by jemalloc")
    (license (list license:expat license:asl2.0))))

(define-public rust-notify-5
  (package
    (name "rust-notify")
    (version "5.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "notify" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02j9pxil0nf766dyj4ha0ss428s07qjkbmh19h11rbmb13d6cb7d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-filetime" ,rust-filetime-0.2)
                       ("rust-fsevent-sys" ,rust-fsevent-sys-4)
                       ("rust-inotify" ,rust-inotify-0.9)
                       ("rust-kqueue" ,rust-kqueue-1)
                       ("rust-kqueue" ,rust-kqueue-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-mio" ,rust-mio-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-walkdir" ,rust-walkdir-2)
                       ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs (("rust-nix" ,rust-nix-0.23)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/notify-rs/notify")
    (synopsis "Cross-platform filesystem notification library")
    (description "Cross-platform filesystem notification library")
    (license (list license:cc0 license:artistic2.0))))

