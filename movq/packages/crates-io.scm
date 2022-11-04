(define-module (movq packages crates-io)
  #:use-module (guix build-system cargo)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages python)
  #:use-module (nongnu packages mozilla)
  #:use-module (movq packages rust))

(define-public rust-version-check-0.9
  (package
    (name "rust-version-check")
    (version "0.9.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "version-check" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gs8grwdlgh0xq660d7wr80x14vxbizmd8dbp29p2pdncx8lp1s9"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/SergioBenitez/version_check")
    (synopsis
     "Tiny crate to check the version of the installed/running rustc.")
    (description
     "Tiny crate to check the version of the installed/running rustc.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ahash-0.8
  (package
    (name "rust-ahash")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ahash" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wldwg3mi8l1lz59c5bv27brz8hkl5rr1m1833gbhbdvrx8ykrjp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-const-random" ,rust-const-random-0.1)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-version-check" ,rust-version-check-0.9))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-fnv" ,rust-fnv-1)
                                   ("rust-fxhash" ,rust-fxhash-0.2)
                                   ("rust-hashbrown" ,rust-hashbrown-0.12)
                                   ("rust-hex" ,rust-hex-0.4)
                                   ("rust-no-panic" ,rust-no-panic-0.1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-seahash" ,rust-seahash-4)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/tkaitchuck/ahash")
    (synopsis
     "A non-cryptographic hash function using AES-NI for high performance")
    (description
     "This package provides a non-cryptographic hash function using AES-NI for high
performance")
    (license (list license:expat license:asl2.0))))

(define-public rust-associative-cache-1
  (package
    (name "rust-associative-cache")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "associative-cache" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "05lg0mwpqfqb9zh958x0358x1k5ngmmmbzjnp0imrd8vzhrn40a6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-rand" ,rust-rand-0.7))))
    (home-page "https://github.com/fitzgen/associative-cache")
    (synopsis
     "A generic N-way associative cache with fixed-size capacity and random or least recently used (LRU) replacement.")
    (description
     "This package provides a generic N-way associative cache with fixed-size capacity
and random or least recently used (LRU) replacement.")
    (license (list license:expat license:asl2.0))))

(define-public rust-compact-str-0.6
  (package
    (name "rust-compact-str")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "compact-str" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fg1x55bdrlhd0jbbpi3ivs8ym3bfsgdqilnl3xpv7lljm9r8f2i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-castaway" ,rust-castaway-0.2)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-markup" ,rust-markup-0.13)
                       ("rust-proptest" ,rust-proptest-1)
                       ("rust-quickcheck" ,rust-quickcheck-1)
                       ("rust-rkyv" ,rust-rkyv-0.7)
                       ("rust-ryu" ,rust-ryu-1)
                       ("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                                   ("rust-proptest" ,rust-proptest-1)
                                   ("rust-quickcheck-macros" ,rust-quickcheck-macros-1)
                                   ("rust-rayon" ,rust-rayon-1)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json" ,rust-serde-json-1)
                                   ("rust-test-strategy" ,rust-test-strategy-0.2))))
    (home-page "https://github.com/ParkMyCar/compact_str")
    (synopsis
     "A memory efficient string type that transparently stores strings on the stack, when possible")
    (description
     "This package provides a memory efficient string type that transparently stores
strings on the stack, when possible")
    (license license:expat)))

(define-public rust-pyo3-ffi-0.17
  (package
    (name "rust-pyo3-ffi")
    (version "0.17.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pyo3-ffi" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0b8kc4ab89rr08fxw56jl5wmpy4ks9j64xyz5nvk4sa89zm2sdch"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-pyo3-build-config" ,rust-pyo3-build-config-0.17))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Python-API bindings for the PyO3 ecosystem")
    (description "Python-API bindings for the PyO3 ecosystem")
    (license license:asl2.0)))

(define-public rust-pyo3-build-config-0.17
  (package
    (name "rust-pyo3-build-config")
    (version "0.17.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pyo3-build-config" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0izgdxdbmcdfsg6lhphsmckjjnlf023f4mphrcsjcs81xp4hh1xz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-once-cell" ,rust-once-cell-1)
                       ("rust-python3-dll-a" ,rust-python3-dll-a-0.2)
                       ("rust-python3-dll-a" ,rust-python3-dll-a-0.2)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.12)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.12))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Build configuration for the PyO3 ecosystem")
    (description "Build configuration for the PyO3 ecosystem")
    (license license:asl2.0)))

(define-public rust-castaway-0.2
  (package
    (name "rust-castaway")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "castaway" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1k1z4v61vq7la56js1azkr0k9b415vif2kaxiqk3d1gw6mbfs5wa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-rustversion" ,rust-rustversion-1))
       #:cargo-development-inputs (("rust-paste" ,rust-paste-1))))
    (home-page "https://github.com/sagebind/castaway")
    (synopsis
     "Safe, zero-cost downcasting for limited compile-time specialization.")
    (description
     "Safe, zero-cost downcasting for limited compile-time specialization.")
    (license license:expat)))

(define-public rust-markup-0.13
  (package
    (name "rust-markup")
    (version "0.13.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "markup" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "03cb086vkqdym3z0wq9h8ywgc2v1w9n4d8pn0j6p76fl6ni9d4dx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-itoa" ,rust-itoa-1)
                       ("rust-markup-proc-macro" ,rust-markup-proc-macro-0.13))))
    (home-page "https://github.com/utkarshkukreti/markup.rs")
    (synopsis "A blazing fast, type-safe template engine for Rust.")
    (description
     "This package provides a blazing fast, type-safe template engine for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rkyv-0.7
  (package
    (name "rust-rkyv")
    (version "0.7.39")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rkyv" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "05gdspzw03hq6l58si4ixfj5xd27ljw6fiqksggnvn87bd4b7hnf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytecheck" ,rust-bytecheck-0.6)
                       ("rust-hashbrown" ,rust-hashbrown-0.12)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-ptr-meta" ,rust-ptr-meta-0.1)
                       ("rust-rend" ,rust-rend-0.3)
                       ("rust-rkyv-derive" ,rust-rkyv-derive-0.7)
                       ("rust-seahash" ,rust-seahash-4)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-tinyvec" ,rust-tinyvec-1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/rkyv/rkyv")
    (synopsis "Zero-copy deserialization framework for Rust")
    (description "Zero-copy deserialization framework for Rust")
    (license license:expat)))

(define-public rust-rkyv-derive-0.7
  (package
    (name "rust-rkyv-derive")
    (version "0.7.39")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rkyv-derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1i1lmir3lm8zj8k1an7j2rchv1admqhysh6r6bfkcgmmi3fdmbkf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/rkyv/rkyv")
    (synopsis "Derive macro for rkyv")
    (description "Derive macro for rkyv")
    (license license:expat)))

(define-public rust-structmeta-0.1
  (package
    (name "rust-structmeta")
    (version "0.1.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "structmeta" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1qyjwgsgllwgi8f9yglv153pr7k81ihrmnc7rg1b57x8b8aw5n8v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-structmeta-derive" ,rust-structmeta-derive-0.1)
                       ("rust-syn" ,rust-syn-1))
       #:cargo-development-inputs (("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/frozenlib/structmeta")
    (synopsis "Parse Rust's attribute arguments by defining a struct.")
    (description "Parse Rust's attribute arguments by defining a struct.")
    (license (list license:expat license:asl2.0))))

(define-public rust-structmeta-derive-0.1
  (package
    (name "rust-structmeta-derive")
    (version "0.1.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "structmeta-derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0f2rgsxfd4asr07svwznh3npcc5fr9c1ayyl6q7r289g186xxzms"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/frozenlib/structmeta")
    (synopsis "derive macro for structmeta crate.")
    (description "derive macro for structmeta crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-test-strategy-0.2
  (package
    (name "rust-test-strategy")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "test-strategy" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "105lxqs0vnqff5821sgns8q1scvrwfx1yw6iz7i7nr862j6l1mk2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-structmeta" ,rust-structmeta-0.1)
                       ("rust-syn" ,rust-syn-1))
       #:cargo-development-inputs (("rust-proptest" ,rust-proptest-1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/frozenlib/test-strategy")
    (synopsis
     "Procedural macro to easily write higher-order strategies in proptest.")
    (description
     "Procedural macro to easily write higher-order strategies in proptest.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bytecheck-0.6
  (package
    (name "rust-bytecheck")
    (version "0.6.9")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "bytecheck" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0vs0a8p3bpaz3vc15zknqkd5ajgzgswf2bmd1mbwdbdm28naq76i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytecheck-derive" ,rust-bytecheck-derive-0.6)
                       ("rust-ptr-meta" ,rust-ptr-meta-0.1)
                       ("rust-simdutf8" ,rust-simdutf8-0.1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/djkoloski/bytecheck")
    (synopsis "Derive macro for bytecheck")
    (description "Derive macro for bytecheck")
    (license license:expat)))

(define-public rust-bytecheck-derive-0.6
  (package
    (name "rust-bytecheck-derive")
    (version "0.6.9")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "bytecheck-derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1gxr63mi91rrjzfzcb8pfwsnarp9i2w1n168nc05aq4fx7mpdr8k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/djkoloski/bytecheck")
    (synopsis "Derive macro for bytecheck")
    (description "Derive macro for bytecheck")
    (license license:expat)))

(define-public rust-rend-0.3
  (package
    (name "rust-rend")
    (version "0.3.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rend" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "15fz3rw8c74586kxl6dcdn4s864ph884wfpg9shgnbrnnss69bvr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytecheck" ,rust-bytecheck-0.6))))
    (home-page "https://github.com/djkoloski/rend")
    (synopsis "Endian-aware primitives for Rust")
    (description "Endian-aware primitives for Rust")
    (license license:expat)))

(define-public rust-markup-proc-macro-0.13
  (package
    (name "rust-markup-proc-macro")
    (version "0.13.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "markup-proc-macro" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0l41di814m9vzw3bz3j49j09j6cb3r73rc4a3a6dvjvx4c77z4hs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/utkarshkukreti/markup.rs")
    (synopsis "A blazing fast, type-safe template engine for Rust.")
    (description
     "This package provides a blazing fast, type-safe template engine for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-packed-simd-2-0.3
  (package
    (name "rust-packed-simd-2")
    (version "0.3.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "packed-simd-2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "10p2bm0p57shg3arlpfwm6z0bbnlkyr4g0dlkmpwvz6qaba4r4d1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-core-arch" ,rust-core-arch-0.1)
                       ("rust-libm" ,rust-libm-0.1)
                       ("rust-sleef-sys" ,rust-sleef-sys-0.1))
       #:cargo-development-inputs (("rust-arrayvec" ,rust-arrayvec-0.5)
                                   ("rust-paste" ,rust-paste-0.1)
                                   ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                                   ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.3))))
    (home-page "https://github.com/rust-lang/packed_simd")
    (synopsis "Portable Packed SIMD vectors")
    (description "Portable Packed SIMD vectors")
    (license (list license:expat license:asl2.0))))

(define-public rust-bytecount-0.6
  (package
    (name "rust-bytecount")
    (version "0.6.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "bytecount" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "173wsvyagflb7ic3hpvp1db6q3dsigr452inslnzmsb3ix3nlrrc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-packed-simd-2" ,rust-packed-simd-2-0.3))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-quickcheck" ,rust-quickcheck-1)
                                   ("rust-rand" ,rust-rand-0.8))))
    (home-page "https://github.com/llogiq/bytecount")
    (synopsis
     "count occurrences of a given byte, or the number of UTF-8 code points, in a byte slice, fast")
    (description
     "count occurrences of a given byte, or the number of UTF-8 code points, in a byte
slice, fast")
    (license (list license:asl2.0 license:expat))))

(define-public python-orjson
 (package
   (name "python-orjson")
   (version "3.8.1")
   (source (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/ijl/orjson")
                    (commit version)))
             (sha256
               (base32
                 "1iljlklvwl0b6cy0nm8sfqcaqxxrnj9nx6qjvidckc4cw8kbnkfx"))
             (file-name (git-file-name name version))))
   (build-system cargo-build-system)
   (native-inputs (list rust-maturin-0.13 python-minimal))
   (arguments
     `(#:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.7)
                       ("rust-ahash" ,rust-ahash-0.8)
                       ("rust-associative-cache" ,rust-associative-cache-1)
                       ("rust-beef" ,rust-beef-0.5)
                       ("rust-bytecount" ,rust-bytecount-0.6)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-compact-str" ,rust-compact-str-0.6)
                       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pyo3-ffi" ,rust-pyo3-ffi-0.17)
                       ("rust-ryu" ,rust-ryu-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-simdutf8" ,rust-simdutf8-0.1)
                       ("rust-smallvec" ,rust-smallvec-1))
       #:phases
       ,#~(modify-phases %standard-phases
            (replace 'build
               (lambda _
                 (invoke "maturin" "build" "--release")))
	    (delete 'package)
            (replace 'install
               (lambda _
		 (let ((filename (car (find-files "target/wheels"))))
                   (invoke "pip3" "install" (string-append "--prefix=" #$output) filename)))))))
    (home-page "https://github.com/ijl/orjson")
    (synopsis
     "Fast, correct Python JSON library supporting dataclasses, datetimes, and numpy")
    (description
     "Fast, correct Python JSON library supporting dataclasses, datetimes, and numpy")
    (license #f)))

(define-public rust-pretty-assertions-1
  (package
    (name "rust-pretty-assertions")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pretty_assertions" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0mgp1ajl3fdc55h989ph48znnk86m41j9dqnpg80yy5a435rnpm2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ctor" ,rust-ctor-0.1)
                       ("rust-diff" ,rust-diff-0.1)
                       ("rust-output-vt100" ,rust-output-vt100-0.1)
                       ("rust-yansi" ,rust-yansi-0.5))))
    (home-page
     "https://github.com/rust-pretty-assertions/rust-pretty-assertions")
    (synopsis
     "Overwrite `assert_eq!` and `assert_ne!` with drop-in replacements, adding colorful diffs.")
    (description
     "Overwrite `assert_eq!` and `assert_ne!` with drop-in replacements, adding
colorful diffs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-valuable-serde-0.1
  (package
    (name "rust-valuable-serde")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "valuable-serde" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "01q9ifpd1mk1ic2g8lagp35djzb8i7cm8skk4rkf5ayd63zwz1aj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-valuable" ,rust-valuable-0.1))))
    (home-page "https://github.com/tokio-rs/valuable")
    (synopsis "`serde::Serialize` implementation for `Valuable` types.")
    (description "`serde::Serialize` implementation for `Valuable` types.")
    (license license:expat)))

(define-public rust-tracing-serde-0.1
  (package
    (name "rust-tracing-serde")
    (version "0.1.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tracing-serde" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1qfr0va69djvxqvjrx4vqq7p6myy414lx4w1f6amcn0hfwqj2sxw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-tracing-core" ,rust-tracing-core-0.1)
                       ("rust-valuable" ,rust-valuable-0.1)
                       ("rust-valuable-serde" ,rust-valuable-serde-0.1))))
    (home-page "https://tokio.rs")
    (synopsis "A compatibility layer for serializing trace data with `serde`
")
    (description
     "This package provides a compatibility layer for serializing trace data with
`serde`")
    (license license:expat)))

(define-public rust-tracing-log-0.1
  (package
    (name "rust-tracing-log")
    (version "0.1.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tracing-log" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "08prnkxq8yas6jvvjnvyx5v3hwblas5527wxxgbiw2yis8rsvpbq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ahash" ,rust-ahash-0.7)
                       ("rust-env-logger" ,rust-env-logger-0.7)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-lru" ,rust-lru-0.7)
                       ("rust-tracing-core" ,rust-tracing-core-0.1))))
    (home-page "https://tokio.rs")
    (synopsis "Provides compatibility between `tracing` and the `log` crate.
")
    (description
     "This package provides compatibility between `tracing` and the `log` crate.")
    (license license:expat)))

(define-public rust-tracing-core-0.1
  (package
    (name "rust-tracing-core")
    (version "0.1.30")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tracing-core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0fi1jz3jbzk3n7k379pwv3wfhn35c5gcwn000m2xh7xb1sx07sr4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-once-cell" ,rust-once-cell-1)
                       ("rust-valuable" ,rust-valuable-0.1))))
    (home-page "https://tokio.rs")
    (synopsis "Core primitives for application-level tracing.
")
    (description "Core primitives for application-level tracing.")
    (license license:expat)))

(define-public rust-thread-local-1
  (package
    (name "rust-thread-local")
    (version "1.1.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "thread_local" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1001bvz6a688wf3izcrh3jqrkiqaarf44wf08azm071ig1xw45jm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-criterion" ,rust-criterion-0.3)
                       ("rust-once-cell" ,rust-once-cell-1))))
    (home-page "https://github.com/Amanieu/thread_local-rs")
    (synopsis "Per-object thread-local storage")
    (description "Per-object thread-local storage")
    (license (list license:asl2.0 license:expat))))

(define-public rust-sharded-slab-0.1
  (package
    (name "rust-sharded-slab")
    (version "0.1.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sharded-slab" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0cbb8kgwsyr3zzhsv8jrs3y1j3vsw4jxil42lfq31ikhdy0bl3wh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-loom" ,rust-loom-0.5))))
    (home-page "https://github.com/hawkw/sharded-slab")
    (synopsis "A lock-free concurrent slab.
")
    (description "This package provides a lock-free concurrent slab.")
    (license license:expat)))

(define-public rust-nu-ansi-term-0.46
  (package
    (name "rust-nu-ansi-term")
    (version "0.46.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "nu-ansi-term" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "115sywxh53p190lyw97alm14nc004qj5jm5lvdj608z84rbida3p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-overload" ,rust-overload-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/nushell/nu-ansi-term")
    (synopsis "Library for ANSI terminal colors and styles (bold, underline)")
    (description
     "Library for ANSI terminal colors and styles (bold, underline)")
    (license license:expat)))

(define-public rust-tracing-subscriber-0.3
  (package
    (name "rust-tracing-subscriber")
    (version "0.3.16")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tracing-subscriber" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0w2sdf97g1ynbmk3j4q6sxmjgaalgf4pg4vl374x0w6x4sp6w5x6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-matchers" ,rust-matchers-0.1)
                       ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.46)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sharded-slab" ,rust-sharded-slab-0.1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-thread-local" ,rust-thread-local-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-core" ,rust-tracing-core-0.1)
                       ("rust-tracing-log" ,rust-tracing-log-0.1)
                       ("rust-tracing-serde" ,rust-tracing-serde-0.1)
                       ("rust-valuable" ,rust-valuable-0.1)
                       ("rust-valuable-serde" ,rust-valuable-serde-0.1))))
    (home-page "https://tokio.rs")
    (synopsis "Utilities for implementing and composing `tracing` subscribers.
")
    (description
     "Utilities for implementing and composing `tracing` subscribers.")
    (license license:expat)))

(define-public rust-textwrap-0.15
  (package
    (name "rust-textwrap")
    (version "0.15.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "textwrap" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0galmidi6gpn308b1kv3r4qbb48j2926lcj0idwhdhlylhjybcxp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-hyphenation" ,rust-hyphenation-0.8)
                       ("rust-smawk" ,rust-smawk-0.3)
                       ("rust-terminal-size" ,rust-terminal-size-0.1)
                       ("rust-unicode-linebreak" ,rust-unicode-linebreak-0.1)
                       ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page "https://github.com/mgeisler/textwrap")
    (synopsis
     "Powerful library for word wrapping, indenting, and dedenting strings")
    (description
     "Powerful library for word wrapping, indenting, and dedenting strings")
    (license license:expat)))

(define-public rust-sha2-0.10
  (package
    (name "rust-sha2")
    (version "0.10.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sha2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1h5xrrv2y06kr1gsz4pwrm3lsp206nm2gjxgbf21wfrfzsavgrl2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cpufeatures" ,rust-cpufeatures-0.2)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-sha2-asm" ,rust-sha2-asm-0.6))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "Pure Rust implementation of the SHA-2 hash function family
including SHA-224, SHA-256, SHA-384, and SHA-512.
")
    (description
     "Pure Rust implementation of the SHA-2 hash function family including SHA-224,
SHA-256, SHA-384, and SHA-512.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rpassword-6
  (package
    (name "rust-rpassword")
    (version "6.0.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rpassword" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0mnrpxvai78mn9wqkqx8wp1gd280jjhn29ixd1dm84l6i2hrkw1b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/conradkleinespel/rpassword")
    (synopsis "Read passwords in console applications.")
    (description "Read passwords in console applications.")
    (license license:asl2.0)))

(define-public rust-xz-0.1
  (package
    (name "rust-xz")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "xz" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0d6sq57g1969hjl5k7gzzdbyr60za9hk8qs9iqz26biazy87d21w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-xz2" ,rust-xz2-0.1))))
    (home-page "https://github.com/alexcrichton/xz2-rs")
    (synopsis
     "Rust bindings to liblzma providing Read/Write streams as well as low-level
in-memory encoding/decoding.

Alias of `xz2` crate.
")
    (description
     "Rust bindings to liblzma providing Read/Write streams as well as low-level
in-memory encoding/decoding.  Alias of `xz2` crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tar-0.4
  (package
    (name "rust-tar")
    (version "0.4.38")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tar" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ikiz14wbfmaaw5mrv93msa8v6n3i595z5kw9p0fdqa40dy80mab"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-filetime" ,rust-filetime-0.2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-xattr" ,rust-xattr-0.2))))
    (home-page "https://github.com/alexcrichton/tar-rs")
    (synopsis
     "A Rust implementation of a TAR file reader and writer. This library does not
currently handle compression, but it is abstract over all I/O readers and
writers. Additionally, great lengths are taken to ensure that the entire
contents are never required to be entirely resident in memory all at once.
")
    (description
     "This package provides a Rust implementation of a TAR file reader and writer.
This library does not currently handle compression, but it is abstract over all
I/O readers and writers.  Additionally, great lengths are taken to ensure that
the entire contents are never required to be entirely resident in memory all at
once.")
    (license (list license:expat license:asl2.0))))

(define-public rust-thiserror-impl-1
  (package
    (name "rust-thiserror-impl")
    (version "1.0.37")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "thiserror-impl" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fydmpksd14x1mkc24zas01qjssz8q43sbn2ywl6n527dda1fbcq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "Implementation detail of the `thiserror` crate")
    (description "Implementation detail of the `thiserror` crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-thiserror-1
  (package
    (name "rust-thiserror")
    (version "1.0.37")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "thiserror" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gky83x4i87gd87w3fknnp920wvk9yycp7dgkf5h3jg364vb7phh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-thiserror-impl" ,rust-thiserror-impl-1))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "derive(Error)")
    (description "derive(Error)")
    (license (list license:expat license:asl2.0))))

(define-public rust-memchr-2
  (package
    (name "rust-memchr")
    (version "2.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "memchr" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0vanfk5mzs1g1syqnj03q8n0syggnhn55dq535h2wxr7rwpfbzrd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/BurntSushi/memchr")
    (synopsis "Safe interface to memchr.")
    (description "Safe interface to memchr.")
    (license (list license:unlicense license:expat))))

(define-public rust-chumsky-0.8
  (package
    (name "rust-chumsky")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "chumsky" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1r09drqc6dh1nha0qhzi6ln552jmnjzyksk8xcdc9il68mp7j0ld"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ahash" ,rust-ahash-0.3))))
    (home-page "https://github.com/zesterer/chumsky")
    (synopsis "A parser library for humans with powerful error recovery")
    (description
     "This package provides a parser library for humans with powerful error recovery")
    (license license:expat)))

(define-public rust-rfc2047-decoder-0.2
  (package
    (name "rust-rfc2047-decoder")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rfc2047-decoder" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "048f0f3pwwf7b62rrh71w9hv2wghgg1r34dgg8v1skp3980psd0i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64" ,rust-base64-0.13)
                       ("rust-charset" ,rust-charset-0.1)
                       ("rust-chumsky" ,rust-chumsky-0.8)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-quoted-printable" ,rust-quoted-printable-0.4)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/soywod/rfc2047-decoder")
    (synopsis "Rust library for decoding RFC 2047 MIME Message Headers.")
    (description "Rust library for decoding RFC 2047 MIME Message Headers.")
    (license license:expat)))

(define-public rust-python-pkginfo-0.5
  (package
    (name "rust-python-pkginfo")
    (version "0.5.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "python-pkginfo" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "022nf2a2kaqa3n217f443dn7inalib36kb5cxdz9c70sk3cg530b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bzip2" ,rust-bzip2-0.4)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-fs-err" ,rust-fs-err-2)
                       ("rust-mailparse" ,rust-mailparse-0.13)
                       ("rust-rfc2047-decoder" ,rust-rfc2047-decoder-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-tar" ,rust-tar-0.4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-xz" ,rust-xz-0.1)
                       ("rust-zip" ,rust-zip-0.6))))
    (home-page "https://github.com/PyO3/python-pkginfo-rs")
    (synopsis "Parse Python package metadata from sdist and bdists and etc.")
    (description
     "Parse Python package metadata from sdist and bdists and etc.")
    (license license:expat)))

(define-public rust-pyproject-toml-0.3
  (package
    (name "rust-pyproject-toml")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pyproject-toml" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1yx8m8a6hf578dxdpv9irrfin2fyfsdyq6bgkxwab24ayznha11q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-toml" ,rust-toml-0.5))))
    (home-page "https://github.com/PyO3/pyproject-toml-rs.git")
    (synopsis "pyproject.toml parser in Rust")
    (description "pyproject.toml parser in Rust")
    (license license:expat)))

(define-public rust-platform-info-1
  (package
    (name "rust-platform-info")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "platform-info" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qsm5wx8mz42sxj78j1n4zjab0zz9xrq99x897ssr7f7a3ynvwlm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/uutils/platform-info")
    (synopsis "A simple cross-platform interface to get info about a system")
    (description
     "This package provides a simple cross-platform interface to get info about a
system")
    (license license:expat)))

(define-public rust-pep440-0.2
  (package
    (name "rust-pep440")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pep440" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12dx79jwmql7nnq1jgayy4ijwh6v26b6sjfdiqz91g7slq6b0hc8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/relrod/pep440-rs")
    (synopsis "Parse and compare Python PEP440 style version numbers")
    (description "Parse and compare Python PEP440 style version numbers")
    (license (list license:bsd-2 license:asl2.0))))

(define-public rust-schannel-0.1
  (package
    (name "rust-schannel")
    (version "0.1.20")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "schannel" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1qnvajc4wg0gzfj4mmg4a9fd45nps5gyvcj4j9fs4bj68q8p7ml8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.36))))
    (home-page "https://github.com/steffengy/schannel-rs")
    (synopsis
     "Schannel bindings for rust, allowing SSL/TLS (e.g. https) without openssl")
    (description
     "Schannel bindings for rust, allowing SSL/TLS (e.g. https) without openssl")
    (license license:expat)))

(define-public rust-openssl-src-300
  (package
    (name "rust-openssl-src")
    (version "300.0.10+3.0.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "openssl-src" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "15pd2cf70z6sg5qzrcls7x5ja722iwr71jn6b7107br7vrdv61n2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/alexcrichton/openssl-src-rs")
    (synopsis "Source of OpenSSL and logic to build it.
")
    (description "Source of OpenSSL and logic to build it.")
    (license (list license:expat license:asl2.0))))

(define-public rust-native-tls-0.2
  (package
    (name "rust-native-tls")
    (version "0.2.10")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "native-tls" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ad4dhkbc3r9rbqdym1cl5zwkqzfa9i8bs0p1c79hzsm30v2yzpx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-openssl" ,rust-openssl-0.10)
                       ("rust-openssl-probe" ,rust-openssl-probe-0.1)
                       ("rust-openssl-src" ,rust-openssl-src-300)
                       ("rust-openssl-sys" ,rust-openssl-sys-0.9)
                       ("rust-schannel" ,rust-schannel-0.1)
                       ("rust-security-framework" ,rust-security-framework-2)
                       ("rust-security-framework-sys" ,rust-security-framework-sys-2)
                       ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/sfackler/rust-native-tls")
    (synopsis "A wrapper over a platform's native TLS implementation")
    (description
     "This package provides a wrapper over a platform's native TLS implementation")
    (license (list license:expat license:asl2.0))))

(define-public rust-ntex-bytes-0.1
  (package
    (name "rust-ntex-bytes")
    (version "0.1.16")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ntex-bytes" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0hangmxzqhwh2948fpsvwwncpmlb9a6813gsp1kf62alvqwbqivm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-simdutf8" ,rust-simdutf8-0.1))))
    (home-page "https://github.com/ntex-rs")
    (synopsis "Types and traits for working with bytes (bytes crate fork)")
    (description "Types and traits for working with bytes (bytes crate fork)")
    (license license:expat)))

(define-public rust-bytes-1
  (package
    (name "rust-bytes")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "bytes" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1nsni0jbx1048inbrarn3hz6zxd000pp0rac2mr07s7xf1m7p2pc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/tokio-rs/bytes")
    (synopsis "Types and traits for working with bytes")
    (description "Types and traits for working with bytes")
    (license license:expat)))

(define-public rust-buf-min-0.7
  (package
    (name "rust-buf-min")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "buf-min" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1hyr2yicp4xlpada6ndwdr3nmh0dykhqg9zqp9hg99b8bazxd8rl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-ntex-bytes" ,rust-ntex-bytes-0.1))))
    (home-page "https://github.com/botika/buf-min")
    (synopsis "Minimal utf-8 safe buffer traits")
    (description "Minimal utf-8 safe buffer traits")
    (license (list license:expat license:asl2.0))))

(define-public rust-v-htmlescape-0.15
  (package
    (name "rust-v-htmlescape")
    (version "0.15.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "v_htmlescape" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "135inp4x7cc32k0hzrymlz1baf0rj0ah5h82nrpa9w0hqpxmg0jf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-buf-min" ,rust-buf-min-0.7))))
    (home-page "https://github.com/botika/v_escape")
    (synopsis "The simd optimized HTML escaping code")
    (description "The simd optimized HTML escaping code")
    (license (list license:expat license:asl2.0))))

(define-public rust-self-cell-0.10
  (package
    (name "rust-self-cell")
    (version "0.10.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "self_cell" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1by8h3axgpbiph5nbq80z6a41hd4cqlqc66hgnngs57y42j6by8y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustversion" ,rust-rustversion-1))))
    (home-page "https://github.com/Voultapher/self_cell")
    (synopsis
     "Safe-to-use proc-macro-free self-referential structs in stable Rust.")
    (description
     "Safe-to-use proc-macro-free self-referential structs in stable Rust.")
    (license license:asl2.0)))

(define-public rust-memo-map-0.3
  (package
    (name "rust-memo-map")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "memo-map" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0h0c80ilf74872nfn1dx65zdj60cxcczrbks113l9kk0jp07dhmf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/mitsuhiko/memo-map")
    (synopsis "A crate implementing a synchronized map for memoization")
    (description
     "This package provides a crate implementing a synchronized map for memoization")
    (license license:asl2.0)))

(define-public rust-minijinja-0.20
  (package
    (name "rust-minijinja")
    (version "0.20.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "minijinja" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0k9qpns2gh71fg1dqqn83hkr7c0h2xc46wbk3liiq82m02zk4325"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-indexmap" ,rust-indexmap-1)
                       ("rust-memo-map" ,rust-memo-map-0.3)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-self-cell" ,rust-self-cell-0.10)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-v-htmlescape" ,rust-v-htmlescape-0.15))))
    (home-page "https://github.com/mitsuhiko/minijinja")
    (synopsis "a powerful template engine for Rust with minimal dependencies")
    (description
     "a powerful template engine for Rust with minimal dependencies")
    (license license:asl2.0)))

(define-public rust-lddtree-0.3
  (package
    (name "rust-lddtree")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "lddtree" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0s99hg1zm990skyih3rid5maa9s4wzkaqyibqw39xk0ff0ns103m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-fs-err" ,rust-fs-err-2)
                       ("rust-glob" ,rust-glob-0.3)
                       ("rust-goblin" ,rust-goblin-0.6))))
    (home-page "https://github.com/messense/lddtree-rs")
    (synopsis "Read the ELF dependency tree")
    (description "Read the ELF dependency tree")
    (license license:expat)))

(define-public rust-security-framework-sys-2
  (package
    (name "rust-security-framework-sys")
    (version "2.6.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "security-framework-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0mn5lm0jip9nm6ydqm6qd9alyiwq15c027777jsbyibs2wxa2q01"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://lib.rs/crates/security-framework-sys")
    (synopsis "Apple `Security.framework` low-level FFI bindings")
    (description "Apple `Security.framework` low-level FFI bindings")
    (license (list license:expat license:asl2.0))))

(define-public rust-core-foundation-sys-0.8
  (package
    (name "rust-core-foundation-sys")
    (version "0.8.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "core-foundation-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1p5r2wckarkpkyc4z83q08dwpvcafrb1h6fxfa3qnikh8szww9sq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Foundation for macOS")
    (description "Bindings to Core Foundation for macOS")
    (license (list license:expat license:asl2.0))))

(define-public rust-core-foundation-0.9
  (package
    (name "rust-core-foundation")
    (version "0.9.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "core-foundation" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ii1ihpjb30fk38gdikm5wqlkmyr8k46fh4k2r8sagz5dng7ljhr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-chrono" ,rust-chrono-0.4)
                       ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-uuid" ,rust-uuid-0.5))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Foundation for macOS")
    (description "Bindings to Core Foundation for macOS")
    (license (list license:expat license:asl2.0))))

(define-public rust-security-framework-2
  (package
    (name "rust-security-framework")
    (version "2.7.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "security-framework" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0v1m0vchbibfr1l0pqiyscp0y7h7f7vkjmy52cc67xjah2bvph9b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-core-foundation" ,rust-core-foundation-0.9)
                       ("rust-core-foundation-sys" ,rust-core-foundation-sys-0.8)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-security-framework-sys" ,rust-security-framework-sys-2))))
    (home-page "https://lib.rs/crates/security_framework")
    (synopsis "Security.framework bindings for macOS and iOS")
    (description "Security.framework bindings for macOS and iOS")
    (license (list license:expat license:asl2.0))))

(define-public rust-zvariant-derive-2
  (package
    (name "rust-zvariant-derive")
    (version "2.10.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zvariant_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1s9xk9c4p9vl0j2vr1abqc12mgv500sjc3fnh8ij3d1yb4i5xjp4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro-crate" ,rust-proc-macro-crate-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://gitlab.freedesktop.org/dbus/zbus/")
    (synopsis "D-Bus & GVariant encoding & decoding")
    (description "D-Bus & GVariant encoding & decoding")
    (license license:expat)))

(define-public rust-zvariant-2
  (package
    (name "rust-zvariant")
    (version "2.10.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zvariant" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0995d59vl8409mk3qrbshqrz5d76dq52szg0x2vqji07y9app356"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.5)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-enumflags2" ,rust-enumflags2-0.6)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-bytes" ,rust-serde-bytes-0.11)
                       ("rust-static-assertions" ,rust-static-assertions-1)
                       ("rust-zvariant-derive" ,rust-zvariant-derive-2))))
    (home-page "https://gitlab.freedesktop.org/dbus/zbus/")
    (synopsis "D-Bus & GVariant encoding & decoding")
    (description "D-Bus & GVariant encoding & decoding")
    (license license:expat)))

(define-public rust-zbus-macros-1
  (package
    (name "rust-zbus-macros")
    (version "1.9.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zbus_macros" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "19p0pdwdf52zkaknav0pj5qvgcf52xk8a4p3a4ymxybwhjkmjfgs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro-crate" ,rust-proc-macro-crate-0.1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://gitlab.freedesktop.org/dbus/zbus/")
    (synopsis "proc-macros for zbus")
    (description "proc-macros for zbus")
    (license license:expat)))

(define-public rust-serde-xml-rs-0.4
  (package
    (name "rust-serde-xml-rs")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "serde-xml-rs" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ykx1xkfd59gf0ijnp93xhpd457xy4zi8xv2hrr0ikvcd6h1pgzh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-xml-rs" ,rust-xml-rs-0.8))))
    (home-page "https://github.com/RReverser/serde-xml-rs")
    (synopsis "xml-rs based deserializer for Serde (compatible with 0.9+)")
    (description "xml-rs based deserializer for Serde (compatible with 0.9+)")
    (license license:expat)))

(define-public rust-nix-0.22
  (package
    (name "rust-nix")
    (version "0.22.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "nix" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1bsgc8vjq07a1wg9vz819bva3dvn58an4r87h80dxrfqkqanz4g4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-memoffset" ,rust-memoffset-0.6))))
    (home-page "https://github.com/nix-rust/nix")
    (synopsis "Rust friendly bindings to *nix APIs")
    (description "Rust friendly bindings to *nix APIs")
    (license license:expat)))

(define-public rust-enumflags2-derive-0.6
  (package
    (name "rust-enumflags2-derive")
    (version "0.6.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "enumflags2_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1kkcwi4n76bi1c16ms00dyk4d393gdf29kpr4k9zsn5z7m7fjvll"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/NieDzejkob/enumflags2")
    (synopsis
     "Do not use directly, use the reexport in the `enumflags2` crate. This allows for better compatibility across versions.")
    (description
     "Do not use directly, use the reexport in the `enumflags2` crate.  This allows
for better compatibility across versions.")
    (license (list license:expat license:asl2.0))))

(define-public rust-enumflags2-0.6
  (package
    (name "rust-enumflags2")
    (version "0.6.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "enumflags2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "182xd6cxxmadx1axnz6x73d12pzgwkc712zq2lxd4z1k48lxij43"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-enumflags2-derive" ,rust-enumflags2-derive-0.6)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/NieDzejkob/enumflags2")
    (synopsis "Enum-based bit flags")
    (description "Enum-based bit flags")
    (license (list license:expat license:asl2.0))))

(define-public rust-zbus-1
  (package
    (name "rust-zbus")
    (version "1.9.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zbus" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0jgwydwjgk16dyrzdbc1k0dnqj9kv9p3fwcv92a7l9np3hlv5glw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-io" ,rust-async-io-1)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-derivative" ,rust-derivative-2)
                       ("rust-enumflags2" ,rust-enumflags2-0.6)
                       ("rust-fastrand" ,rust-fastrand-1)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-nb-connect" ,rust-nb-connect-1)
                       ("rust-nix" ,rust-nix-0.22)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-polling" ,rust-polling-2)
                       ("rust-scoped-tls" ,rust-scoped-tls-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-xml-rs" ,rust-serde-xml-rs-0.4)
                       ("rust-serde-repr" ,rust-serde-repr-0.1)
                       ("rust-zbus-macros" ,rust-zbus-macros-1)
                       ("rust-zvariant" ,rust-zvariant-2))))
    (home-page "https://gitlab.freedesktop.org/dbus/zbus/")
    (synopsis "API for D-Bus communication")
    (description "API for D-Bus communication")
    (license license:expat)))

(define-public rust-secret-service-2
  (package
    (name "rust-secret-service")
    (version "2.0.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "secret-service" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18l0yz9sb062jddcx56qi70d4ry2js3irkgysdgii0w77d15rnp1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aes" ,rust-aes-0.7)
                       ("rust-block-modes" ,rust-block-modes-0.8)
                       ("rust-hkdf" ,rust-hkdf-0.11)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-num" ,rust-num-0.4)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha2" ,rust-sha2-0.9)
                       ("rust-zbus" ,rust-zbus-1)
                       ("rust-zbus-macros" ,rust-zbus-macros-1)
                       ("rust-zvariant" ,rust-zvariant-2)
                       ("rust-zvariant-derive" ,rust-zvariant-derive-2))))
    (home-page "https://github.com/hwchen/secret-service-rs.git")
    (synopsis "Library to interface with Secret Service API")
    (description "Library to interface with Secret Service API")
    (license (list license:expat license:asl2.0))))

(define-public rust-keyring-1
  (package
    (name "rust-keyring")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "keyring" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1r8hka1hkb9li0f7qybpb5dhngk5y288syjpfjrcrgyavncq7yrq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-secret-service" ,rust-secret-service-2)
                       ("rust-security-framework" ,rust-security-framework-2)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/hwchen/keyring-rs")
    (synopsis "Cross-platform library for managing passwords/credentials")
    (description "Cross-platform library for managing passwords/credentials")
    (license (list license:expat license:asl2.0))))

(define-public rust-os-type-2
  (package
    (name "rust-os-type")
    (version "2.6.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "os_type" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "19bv5jq9z04bw3kf9qdxw76yngjy9g5dmxnqdr8nf0d3xv048kg2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/schultyy/os_type")
    (synopsis "Detect the operating system type")
    (description "Detect the operating system type")
    (license license:expat)))

(define-public rust-human-panic-1
  (package
    (name "rust-human-panic")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "human-panic" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0djfad84iwl86kabj8rqfhv5nn1qi1fd9hb7z72xgjxb02jmgwrr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-os-type" ,rust-os-type-2)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-termcolor" ,rust-termcolor-1)
                       ("rust-toml" ,rust-toml-0.5)
                       ("rust-uuid" ,rust-uuid-0.8))))
    (home-page "https://github.com/yoshuawuyts/human-panic")
    (synopsis "Panic messages for humans")
    (description "Panic messages for humans")
    (license (list license:expat license:asl2.0))))

(define-public rust-goblin-0.5
  (package
    (name "rust-goblin")
    (version "0.5.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "goblin" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0hs1npqx1nx4fwjm59c1n9pr581w0l0fwxk5dwdd5n0dxn1njrm7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-plain" ,rust-plain-0.2)
                       ("rust-scroll" ,rust-scroll-0.11))))
    (home-page "https://github.com/m4b/goblin")
    (synopsis
     "An impish, cross-platform, ELF, Mach-o, and PE binary parsing and loading crate")
    (description
     "An impish, cross-platform, ELF, Mach-o, and PE binary parsing and loading crate")
    (license license:expat)))

(define-public rust-llvm-bitcode-0.1
  (package
    (name "rust-llvm-bitcode")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "llvm-bitcode" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fskb2nzddg8m038kh2lm91gb5yg0l4j3rcnv44kz7f37kcxz5cb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-num-enum" ,rust-num-enum-0.5))))
    (home-page "https://github.com/messense/llvm-bitcode-rs.git")
    (synopsis "LLVM Bitcode parser in Rust")
    (description "LLVM Bitcode parser in Rust")
    (license license:expat)))

(define-public rust-scroll-derive-0.11
  (package
    (name "rust-scroll-derive")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "scroll_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "03i5qn4jfcl2iwxhfvw9kf48a656ycbf5km99xr1wcnibjnadgdx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/m4b/scroll")
    (synopsis
     "A macros 1.1 derive implementation for Pread and Pwrite traits from the scroll crate")
    (description
     "This package provides a macros 1.1 derive implementation for Pread and Pwrite
traits from the scroll crate")
    (license license:expat)))

(define-public rust-scroll-0.11
  (package
    (name "rust-scroll")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "scroll" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1nhrhpzf95pxbcjjy222blwf8rl3adws6vsqax0yzyxsa6snbi84"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-scroll-derive" ,rust-scroll-derive-0.11))))
    (home-page "https://github.com/m4b/scroll")
    (synopsis
     "A suite of powerful, extensible, generic, endian-aware Read/Write traits for byte buffers")
    (description
     "This package provides a suite of powerful, extensible, generic, endian-aware
Read/Write traits for byte buffers")
    (license license:expat)))

(define-public rust-goblin-0.6
  (package
    (name "rust-goblin")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "goblin" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "01cmmjh85q91zc1b92jp6spdad6mqkmpx3ic41srgl57rgb689ap"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-plain" ,rust-plain-0.2)
                       ("rust-scroll" ,rust-scroll-0.11))))
    (home-page "https://github.com/m4b/goblin")
    (synopsis
     "An impish, cross-platform, ELF, Mach-o, and PE binary parsing and loading crate")
    (description
     "An impish, cross-platform, ELF, Mach-o, and PE binary parsing and loading crate")
    (license license:expat)))

(define-public rust-fat-macho-0.4
  (package
    (name "rust-fat-macho")
    (version "0.4.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "fat-macho" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0jfib3z5vzs45f6lfm2lvw8rq17g01310n3a5fsc4i5rl8qp3w37"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-goblin" ,rust-goblin-0.6)
                       ("rust-llvm-bitcode" ,rust-llvm-bitcode-0.1))))
    (home-page "https://github.com/messense/fat-macho-rs.git")
    (synopsis "Mach-O Fat Binary Reader and Writer")
    (description "Mach-O Fat Binary Reader and Writer")
    (license license:expat)))

(define-public rust-dunce-1
  (package
    (name "rust-dunce")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dunce" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0g0wng3v9z0sh0b756wawm40ixhl7x6f6k8gcasdkfv0cl5b7m0b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://lib.rs/crates/dunce")
    (synopsis
     "Normalize Windows paths to the most compatible format, avoiding UNC where possible")
    (description
     "Normalize Windows paths to the most compatible format, avoiding UNC where
possible")
    (license (list license:cc0 license:expat-0))))

(define-public rust-dialoguer-0.10
  (package
    (name "rust-dialoguer")
    (version "0.10.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "dialoguer" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ccf0xnhlcfxjb68688cb538x4xhslpx9if0q3ymfs7gxhvpwbm9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-console" ,rust-console-0.15)
                       ("rust-fuzzy-matcher" ,rust-fuzzy-matcher-0.3)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/mitsuhiko/dialoguer")
    (synopsis "A command line prompting library.")
    (description "This package provides a command line prompting library.")
    (license license:expat)))

(define-public rust-console-0.15
  (package
    (name "rust-console")
    (version "0.15.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "console" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "133ik2r2ynk55007dig05hlzjgl6kmhqqpdn0iy73vbwjrykcl60"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-encode-unicode" ,rust-encode-unicode-0.3)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-terminal-size" ,rust-terminal-size-0.1)
                       ("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-winapi" ,rust-winapi-0.3)
                       ("rust-winapi-util" ,rust-winapi-util-0.1))))
    (home-page "https://github.com/console-rs/console")
    (synopsis "A terminal and console abstraction for Rust")
    (description
     "This package provides a terminal and console abstraction for Rust")
    (license license:expat)))

(define-public rust-rustc-rayon-core-0.4
  (package
    (name "rust-rustc-rayon-core")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustc-rayon-core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0c4cf58056ya3282c24bnyq39cwm1rd1m96lymfbb6yvl12929h2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-crossbeam-deque" ,rust-crossbeam-deque-0.8)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-num-cpus" ,rust-num-cpus-1))))
    (home-page "https://github.com/rust-lang/rustc-rayon")
    (synopsis "Core APIs for Rayon - fork for rustc")
    (description "Core APIs for Rayon - fork for rustc")
    (license (list license:expat license:asl2.0))))

(define-public rust-crossbeam-deque-0.8
  (package
    (name "rust-crossbeam-deque")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "crossbeam-deque" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1z6ifz35lyk0mw818xcl3brgss2k8islhgdmfk9s5fwjnr982pki"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-crossbeam-epoch" ,rust-crossbeam-epoch-0.9)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8))))
    (home-page
     "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-deque")
    (synopsis "Concurrent work-stealing deque")
    (description "Concurrent work-stealing deque")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustc-rayon-0.4
  (package
    (name "rust-rustc-rayon")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustc-rayon" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ykjr1i56jmi8ykkcr7x555wnxki1vsi703mz6n2x7k0naqg0y8s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-crossbeam-deque" ,rust-crossbeam-deque-0.8)
                       ("rust-either" ,rust-either-1)
                       ("rust-rustc-rayon-core" ,rust-rustc-rayon-core-0.4))))
    (home-page "https://github.com/rust-lang/rustc-rayon")
    (synopsis "Simple work-stealing parallelism for Rust - fork for rustc")
    (description "Simple work-stealing parallelism for Rust - fork for rustc")
    (license (list license:expat license:asl2.0))))

(define-public rust-indexmap-1
  (package
    (name "rust-indexmap")
    (version "1.9.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "indexmap" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "07nli1wcz7m81svvig8l5j6vjycjnv9va46lwblgy803ffbmm8qh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-hashbrown" ,rust-hashbrown-0.12)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rustc-rayon" ,rust-rustc-rayon-0.4)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/bluss/indexmap")
    (synopsis "A hash table with consistent order and fast iteration.")
    (description
     "This package provides a hash table with consistent order and fast iteration.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-configparser-3
  (package
    (name "rust-configparser")
    (version "3.0.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "configparser" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ypq3phwrlx2c7agdj1rlivkhsk9k795jb30j58azvw7lp8xjn2l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-indexmap" ,rust-indexmap-1))))
    (home-page "https://github.com/QEDK/configparser-rs")
    (synopsis
     "A simple configuration parsing utility with no dependencies that allows you to parse INI and ini-style syntax. You can use this to write Rust programs which can be customized by end users easily.")
    (description
     "This package provides a simple configuration parsing utility with no
dependencies that allows you to parse INI and ini-style syntax.  You can use
this to write Rust programs which can be customized by end users easily.")
    (license (list license:expat license:lgpl3+))))

(define-public rust-clap-complete-fig-3
  (package
    (name "rust-clap-complete-fig")
    (version "3.2.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "clap_complete_fig" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fb4965w8wyrcwq35ywgx4mzfsv2cqba73mdlvmp6ii1q70b8dzd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-clap" ,rust-clap-3)
                       ("rust-clap-complete" ,rust-clap-complete-3))))
    (home-page "https://github.com/clap-rs/clap/tree/master/clap_complete_fig")
    (synopsis "A generator library used with clap for Fig completion scripts")
    (description
     "This package provides a generator library used with clap for Fig completion
scripts")
    (license (list license:expat license:asl2.0))))

(define-public rust-unicode-xid-0.2
  (package
    (name "rust-unicode-xid")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "unicode-xid" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "131dfzf7d8fsr1ivch34x42c2d1ik5ig3g78brxncnn0r1sdyqpr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/unicode-rs/unicode-xid")
    (synopsis "Determine whether characters have the XID_Start
or XID_Continue properties according to
Unicode Standard Annex #31.
")
    (description
     "Determine whether characters have the XID_Start or XID_Continue properties
according to Unicode Standard Annex #31.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pathdiff-0.2
  (package
    (name "rust-pathdiff")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pathdiff" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pa4dcmb7lwir4himg1mnl97a05b2z0svczg62l8940pbim12dc8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-camino" ,rust-camino-1))))
    (home-page "https://github.com/Manishearth/pathdiff")
    (synopsis "Library for diffing paths to obtain relative paths")
    (description "Library for diffing paths to obtain relative paths")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-complete-3
  (package
    (name "rust-clap-complete")
    (version "3.2.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "clap_complete" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1n3whjkznszrxif1hzvql7hav7agq85j456fmwjwwi9cjq52wyiz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-clap" ,rust-clap-3)
                       ("rust-clap-lex" ,rust-clap-lex-0.2)
                       ("rust-is-executable" ,rust-is-executable-1)
                       ("rust-os-str-bytes" ,rust-os-str-bytes-6)
                       ("rust-pathdiff" ,rust-pathdiff-0.2)
                       ("rust-shlex" ,rust-shlex-1)
                       ("rust-unicode-xid" ,rust-unicode-xid-0.2))))
    (home-page "https://github.com/clap-rs/clap/tree/master/clap_complete")
    (synopsis "Generate shell completion scripts for your clap::Command")
    (description "Generate shell completion scripts for your clap::Command")
    (license (list license:expat license:asl2.0))))

(define-public rust-unicode-linebreak-0.1
  (package
    (name "rust-unicode-linebreak")
    (version "0.1.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "unicode-linebreak" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0drixqb16bzmabd5d8ldvar5760rxy6nxzszhlsqnasl3bisvyn5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-hashbrown" ,rust-hashbrown-0.12)
                       ("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/axelf4/unicode-linebreak")
    (synopsis "Implementation of the Unicode Line Breaking Algorithm")
    (description "Implementation of the Unicode Line Breaking Algorithm")
    (license license:asl2.0)))

(define-public rust-textwrap-0.16
  (package
    (name "rust-textwrap")
    (version "0.16.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "textwrap" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gbwkjf15l6p3x2rkr75fa4cpcs1ly4c8pmlfx5bl6zybcm24ai2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-hyphenation" ,rust-hyphenation-0.8)
                       ("rust-smawk" ,rust-smawk-0.3)
                       ("rust-terminal-size" ,rust-terminal-size-0.2)
                       ("rust-unicode-linebreak" ,rust-unicode-linebreak-0.1)
                       ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page "https://github.com/mgeisler/textwrap")
    (synopsis
     "Powerful library for word wrapping, indenting, and dedenting strings")
    (description
     "Powerful library for word wrapping, indenting, and dedenting strings")
    (license license:expat)))

(define-public rust-clap-derive-3
  (package
    (name "rust-clap-derive")
    (version "3.2.18")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "clap_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0r9az0cl33xx0i9g18l56l3vd5ayjvcflvza2gdf8jwcab78n37a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-heck" ,rust-heck-0.4)
                       ("rust-proc-macro-error" ,rust-proc-macro-error-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/clap-rs/clap/tree/master/clap_derive")
    (synopsis
     "Parse command line argument by defining a struct, derive crate.")
    (description
     "Parse command line argument by defining a struct, derive crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-3
  (package
    (name "rust-clap")
    (version "3.2.23")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "clap" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "19bkwkj49ha7mlip0gxsqb9xmd3jpr7ghvcx1hkx6icqrd2mqrbi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atty" ,rust-atty-0.2)
                       ("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-clap-derive" ,rust-clap-derive-3)
                       ("rust-clap-lex" ,rust-clap-lex-0.2)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-strsim" ,rust-strsim-0.10)
                       ("rust-termcolor" ,rust-termcolor-1)
                       ("rust-terminal-size" ,rust-terminal-size-0.2)
                       ("rust-textwrap" ,rust-textwrap-0.16)
                       ("rust-unicase" ,rust-unicase-2)
                       ("rust-yaml-rust" ,rust-yaml-rust-0.4))))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis
     "A simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
     "This package provides a simple to use, efficient, and full-featured Command Line
Argument Parser")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-json-1
  (package
    (name "rust-serde-json")
    (version "1.0.87")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "serde_json" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ibxrq43axvspv350wvx7w05l4s7b1gvaa0dysf6pmshn6vpgrvc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-indexmap" ,rust-indexmap-1)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-ryu" ,rust-ryu-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/serde-rs/json")
    (synopsis "A JSON serialization file format")
    (description "This package provides a JSON serialization file format")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-derive-1
  (package
    (name "rust-serde-derive")
    (version "1.0.147")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "serde_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ln8rqbybpxmk4fvh6lgm75acs1d8x90fi44fhx3x77wm0n3c7ag"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://serde.rs")
    (synopsis "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
     "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-1
  (package
    (name "rust-serde")
    (version "1.0.147")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "serde" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rc9jj8bbhf3lkf07ln8kyljigyzc4kk90nzg4dc2gwqmsdxd4yi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://serde.rs")
    (synopsis "A generic serialization/deserialization framework")
    (description
     "This package provides a generic serialization/deserialization framework")
    (license (list license:expat license:asl2.0))))

(define-public rust-semver-1
  (package
    (name "rust-semver")
    (version "1.0.14")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "semver" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1i193dd6xkhh2fi1x7rws9pvv2ff3jfl9qjvvd9y6y6pcg2glpg2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/dtolnay/semver")
    (synopsis "Parser and evaluator for Cargo's flavor of Semantic Versioning")
    (description
     "Parser and evaluator for Cargo's flavor of Semantic Versioning")
    (license (list license:expat license:asl2.0))))

(define-public rust-cargo-zigbuild-0.12
  (package
    (name "rust-cargo-zigbuild")
    (version "0.12.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cargo-zigbuild" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1bapmr0skg1rzfl07mm4i6kbqcvwkgxs2j0m0k35cw04b3p5yn8q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-cargo-options" ,rust-cargo-options-0.3)
                       ("rust-cargo-metadata" ,rust-cargo-metadata-0.15)
                       ("rust-clap" ,rust-clap-3)
                       ("rust-dirs" ,rust-dirs-4)
                       ("rust-fs-err" ,rust-fs-err-2)
                       ("rust-path-slash" ,rust-path-slash-0.2)
                       ("rust-rustc-version" ,rust-rustc-version-0.4)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.12)
                       ("rust-which" ,rust-which-4))))
    (home-page "https://github.com/messense/cargo-zigbuild")
    (synopsis "Compile Cargo project with zig as linker")
    (description "Compile Cargo project with zig as linker")
    (license license:expat)))

(define-public rust-zstd-sys-2
  (package
    (name "rust-zstd-sys")
    (version "2.0.1+zstd.1.5.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zstd-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0azifd7xsyy9yljihx26pr9am85717fzdzdzbladjiiqqnxprl4z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.59)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/gyscos/zstd-rs")
    (synopsis "Low-level bindings for the zstd compression library.")
    (description "Low-level bindings for the zstd compression library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-zstd-safe-5
  (package
    (name "rust-zstd-safe")
    (version "5.0.2+zstd.1.5.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zstd-safe" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1nzl4q3xl68pq58g9xlym299bvjdii8cl7ix595ym7jgw22maahx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-zstd-sys" ,rust-zstd-sys-2))))
    (home-page "https://github.com/gyscos/zstd-rs")
    (synopsis "Safe low-level bindings for the zstd compression library.")
    (description "Safe low-level bindings for the zstd compression library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-zstd-0.11
  (package
    (name "rust-zstd")
    (version "0.11.2+zstd.1.5.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zstd" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1r7xlmgnifhxbfyid8vkcnd5ip16gx9hf89d1l0lzrpc4q1rdk10"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-zstd-safe" ,rust-zstd-safe-5))))
    (home-page "https://github.com/gyscos/zstd-rs")
    (synopsis "Binding for the zstd compression library.")
    (description "Binding for the zstd compression library.")
    (license license:expat)))

(define-public rust-time-macros-0.2
  (package
    (name "rust-time-macros")
    (version "0.2.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "time-macros" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0nqff5j5170llixk05vb3x76xri63x9znavxmrica4nq64c81fv5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-time-core" ,rust-time-core-0.1))))
    (home-page "https://github.com/time-rs/time")
    (synopsis
     "    Procedural macros for the time crate.
    This crate is an implementation detail and should not be relied upon directly.
")
    (description
     "Procedural macros for the time crate.  This crate is an implementation detail
and should not be relied upon directly.")
    (license (list license:expat license:asl2.0))))

(define-public rust-time-core-0.1
  (package
    (name "rust-time-core")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "time-core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1z803zwzyh16nk3c4nmkw8v69nyj0r4v8s3yag68mvya38gkw59f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/time-rs/time")
    (synopsis
     "This crate is an implementation detail and should not be relied upon directly.")
    (description
     "This crate is an implementation detail and should not be relied upon directly.")
    (license (list license:expat license:asl2.0))))

(define-public rust-num-threads-0.1
  (package
    (name "rust-num-threads")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "num_threads" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0i5vmffsv6g79z869flp1sja69g1gapddjagdw1k3q9f3l2cw698"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/jhpratt/num_threads")
    (synopsis
     "A minimal library that determines the number of running threads for the current process.")
    (description
     "This package provides a minimal library that determines the number of running
threads for the current process.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-shared-0.2
  (package
    (name "rust-wasm-bindgen-shared")
    (version "0.2.83")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wasm-bindgen-shared" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0zzz9xfi3fp2n5ihhlq8ws7674a2ir2frvsd1d7yr4sxad2w0f0w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
     "Shared support between wasm-bindgen and wasm-bindgen cli, an internal
dependency.
")
    (description
     "Shared support between wasm-bindgen and wasm-bindgen cli, an internal
dependency.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-backend-0.2
  (package
    (name "rust-wasm-bindgen-backend")
    (version "0.2.83")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wasm-bindgen-backend" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0hhigjqrb31axh7jgmb5y8akdpxqx8gvjs6ja9xmbc3r4lrzp3sc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bumpalo" ,rust-bumpalo-3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-wasm-bindgen-shared" ,rust-wasm-bindgen-shared-0.2))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Backend code generation of the wasm-bindgen tool
")
    (description "Backend code generation of the wasm-bindgen tool")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-macro-support-0.2
  (package
    (name "rust-wasm-bindgen-macro-support")
    (version "0.2.83")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wasm-bindgen-macro-support" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0g0rmawgkhfyfgjj2mvch7gvz1nzfnfmya0kgcq3xwn53l2hrg07"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-wasm-bindgen-backend" ,rust-wasm-bindgen-backend-0.2)
                       ("rust-wasm-bindgen-shared" ,rust-wasm-bindgen-shared-0.2))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
     "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in the shared backend crate
")
    (description
     "The part of the implementation of the `#[wasm_bindgen]` attribute that is not in
the shared backend crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-macro-0.2
  (package
    (name "rust-wasm-bindgen-macro")
    (version "0.2.83")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wasm-bindgen-macro" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0468wshk7bp78mnglcpmrb6m4q7x2fp9pz6ybk3wpri683wy0aq5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-quote" ,rust-quote-1)
                       ("rust-wasm-bindgen-macro-support" ,rust-wasm-bindgen-macro-support-0.2))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
     "Definition of the `#[wasm_bindgen]` attribute, an internal dependency
")
    (description
     "Definition of the `#[wasm_bindgen]` attribute, an internal dependency")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-0.2
  (package
    (name "rust-wasm-bindgen")
    (version "0.2.83")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wasm-bindgen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0s3ji0k8p261glnsxi5rkd34v2pv67h96blb29yf32zcxsngbyga"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-wasm-bindgen-macro" ,rust-wasm-bindgen-macro-0.2))))
    (home-page "https://rustwasm.github.io/")
    (synopsis "Easy support for interacting between JS and Rust.
")
    (description "Easy support for interacting between JS and Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-js-sys-0.3
  (package
    (name "rust-js-sys")
    (version "0.3.60")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "js-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0isslargvb1cd5xfk73xrxqni3p2ksharkp22swmc25zwgrrsh29"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
     "Bindings for all JS global objects and functions in all JS environments like
Node.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate.
")
    (description
     "Bindings for all JS global objects and functions in all JS environments like
Node.js and browsers, built on `#[wasm_bindgen]` using the `wasm-bindgen` crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-time-0.3
  (package
    (name "rust-time")
    (version "0.3.16")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "time" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jpv33lwhvxxibxw09rr3y02q1lwhfmy7nrdv430x1c0k65mraqg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-itoa" ,rust-itoa-1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-num-threads" ,rust-num-threads-0.1)
                       ("rust-quickcheck" ,rust-quickcheck-1)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-time-core" ,rust-time-core-0.1)
                       ("rust-time-macros" ,rust-time-macros-0.2))))
    (home-page "https://time-rs.github.io")
    (synopsis
     "Date and time library. Fully interoperable with the standard library. Mostly compatible with #![no_std].")
    (description
     "Date and time library.  Fully interoperable with the standard library.  Mostly
compatible with #![no_std].")
    (license (list license:expat license:asl2.0))))

(define-public rust-sha1-0.10
  (package
    (name "rust-sha1")
    (version "0.10.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sha1" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18zb80sxn31kxdpl1ly6w17hkrvyf08zbxnpy8ckb6f3h3f96hph"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cfg-if" ,rust-cfg-if-1)
                       ("rust-cpufeatures" ,rust-cpufeatures-0.2)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-sha1-asm" ,rust-sha1-asm-0.5))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "SHA-1 hash function")
    (description "SHA-1 hash function")
    (license (list license:expat license:asl2.0))))

(define-public rust-password-hash-0.4
  (package
    (name "rust-password-hash")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "password-hash" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "003p2hssyrcaxyq9fs8x2wx5di8ny9byaakskrf352pfm963fxkn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base64ct" ,rust-base64ct-1)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-subtle" ,rust-subtle-2))))
    (home-page
     "https://github.com/RustCrypto/traits/tree/master/password-hash")
    (synopsis
     "Traits which describe the functionality of password hashing algorithms,
as well as a `no_std`-friendly implementation of the PHC string format
(a well-defined subset of the Modular Crypt Format a.k.a. MCF)
")
    (description
     "Traits which describe the functionality of password hashing algorithms, as well
as a `no_std`-friendly implementation of the PHC string format (a well-defined
subset of the Modular Crypt Format a.k.a.  MCF)")
    (license (list license:expat license:asl2.0))))

(define-public rust-pbkdf2-0.11
  (package
    (name "rust-pbkdf2")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "pbkdf2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "05q9wqjvfrs4dvw03yn3bvcs4zghz0a7ycfa53pz2k2fqhp6k843"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-digest" ,rust-digest-0.10)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-password-hash" ,rust-password-hash-0.4)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-sha-1" ,rust-sha-1-0.10)
                       ("rust-sha2" ,rust-sha2-0.10))))
    (home-page
     "https://github.com/RustCrypto/password-hashes/tree/master/pbkdf2")
    (synopsis "Generic implementation of PBKDF2")
    (description "Generic implementation of PBKDF2")
    (license (list license:expat license:asl2.0))))

(define-public rust-hmac-0.12
  (package
    (name "rust-hmac")
    (version "0.12.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hmac" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pmbr069sfg76z7wsssfk5ddcqd9ncp79fyz6zcm6yn115yc6jbc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-digest" ,rust-digest-0.10))))
    (home-page "https://github.com/RustCrypto/MACs")
    (synopsis
     "Generic implementation of Hash-based Message Authentication Code (HMAC)")
    (description
     "Generic implementation of Hash-based Message Authentication Code (HMAC)")
    (license (list license:expat license:asl2.0))))

(define-public rust-bzip2-sys-0.1
  (package
    (name "rust-bzip2-sys")
    (version "0.1.11+1.0.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "bzip2-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1p2crnv8d8gpz5c2vlvzl0j55i3yqg5bi0kwsl1531x77xgraskk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/alexcrichton/bzip2-rs")
    (synopsis
     "Bindings to libbzip2 for bzip2 compression and decompression exposed as
Reader/Writer streams.
")
    (description
     "Bindings to libbzip2 for bzip2 compression and decompression exposed as
Reader/Writer streams.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bzip2-0.4
  (package
    (name "rust-bzip2")
    (version "0.4.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "bzip2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1c495c2zh3knxwby2v1m7b21qddvrkya4mvyqlbm197knn0dkz3a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bzip2-sys" ,rust-bzip2-sys-0.1)
                       ("rust-futures" ,rust-futures-0.1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-tokio-io" ,rust-tokio-io-0.1))))
    (home-page "https://github.com/alexcrichton/bzip2-rs")
    (synopsis
     "Bindings to libbzip2 for bzip2 compression and decompression exposed as
Reader/Writer streams.
")
    (description
     "Bindings to libbzip2 for bzip2 compression and decompression exposed as
Reader/Writer streams.")
    (license (list license:expat license:asl2.0))))

(define-public rust-zip-0.6
  (package
    (name "rust-zip")
    (version "0.6.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zip" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "107hpmvcqrijxjbxclavy77vzrs0b2qy0z0swa54xr953m0yfz2k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-aes" ,rust-aes-0.7)
                       ("rust-byteorder" ,rust-byteorder-1)
                       ("rust-bzip2" ,rust-bzip2-0.4)
                       ("rust-constant-time-eq" ,rust-constant-time-eq-0.1)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-pbkdf2" ,rust-pbkdf2-0.11)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-zstd" ,rust-zstd-0.11))))
    (home-page "https://github.com/zip-rs/zip.git")
    (synopsis "Library to support the reading and writing of zip files.
")
    (description "Library to support the reading and writing of zip files.")
    (license license:expat)))

(define-public rust-autocfg-1
  (package
    (name "rust-autocfg")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "autocfg" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ylp3cb47ylzabimazvbz9ms6ap784zhb6syaz6c1jqpmcmq0s6l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/cuviper/autocfg")
    (synopsis "Automatic cfg for Rust compiler features")
    (description "Automatic cfg for Rust compiler features")
    (license (list license:asl2.0 license:expat))))

(define-public rust-lock-api-0.4
  (package
    (name "rust-lock-api")
    (version "0.4.9")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "lock_api" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1py41vk243hwk345nhkn5nw0bd4m03gzjmprdjqq6rg5dwv12l23"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-autocfg" ,rust-autocfg-1)
                       ("rust-owning-ref" ,rust-owning-ref-0.4)
                       ("rust-scopeguard" ,rust-scopeguard-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/Amanieu/parking_lot")
    (synopsis
     "Wrappers to create fully-featured Mutex and RwLock types. Compatible with no_std.")
    (description
     "Wrappers to create fully-featured Mutex and RwLock types.  Compatible with
no_std.")
    (license (list license:expat license:asl2.0))))

(define-public rust-parking-lot-0.12
  (package
    (name "rust-parking-lot")
    (version "0.12.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "parking_lot" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "13r2xk7mnxfc5g0g6dkdxqdqad99j7s7z8zhzz4npw5r0g0v4hip"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lock-api" ,rust-lock-api-0.4)
                       ("rust-parking-lot-core" ,rust-parking-lot-core-0.9))))
    (home-page "https://github.com/Amanieu/parking_lot")
    (synopsis
     "More compact and efficient implementations of the standard synchronization primitives.")
    (description
     "More compact and efficient implementations of the standard synchronization
primitives.")
    (license (list license:expat license:asl2.0))))

(define-public rust-zerocopy-derive-0.3
  (package
    (name "rust-zerocopy-derive")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zerocopy-derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18qr7dqlj89v1xl1g58l2xd6jidv0sbccscgl131gpppba0yc1b5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/google/zerocopy")
    (synopsis "Custom derive for traits from the zerocopy crate")
    (description "Custom derive for traits from the zerocopy crate")
    (license #f)))

(define-public rust-zerocopy-0.6
  (package
    (name "rust-zerocopy")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "zerocopy" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0dpj4nd9v56wy93ahjkp95znjzj91waqvidqch8gxwdwq661hbrk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-zerocopy-derive" ,rust-zerocopy-derive-0.3))))
    (home-page "https://github.com/google/zerocopy")
    (synopsis "Utilities for zero-copy parsing and serialization")
    (description "Utilities for zero-copy parsing and serialization")
    (license #f)))

(define-public rust-uuid-macro-internal-1
  (package
    (name "rust-uuid-macro-internal")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "uuid-macro-internal" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0jkww3arqgqfm90l1ynyq531kjqn8mbbwyskg3qa4prp1idk1k24"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "")
    (synopsis "Private implementation details of the uuid! macro.")
    (description "Private implementation details of the uuid! macro.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-sha1-smol-1
  (package
    (name "rust-sha1-smol")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "sha1_smol" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04nhbhvsk5ms1zbshs80iq5r1vjszp2xnm9f0ivj38q3dhc4f6mf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/mitsuhiko/sha1-smol")
    (synopsis "Minimal dependency free implementation of SHA1 for Rust.")
    (description "Minimal dependency free implementation of SHA1 for Rust.")
    (license license:bsd-3)))

(define-public rust-md5-asm-0.5
  (package
    (name "rust-md5-asm")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "md5-asm" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ixmkg8j7sqy9zln6pz9xi2dl2d9zpm8pz6p49za47n1bvradfbk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1))))
    (home-page "https://github.com/RustCrypto/asm-hashes")
    (synopsis "Assembly implementation of MD5 compression function")
    (description "Assembly implementation of MD5 compression function")
    (license license:expat)))

(define-public rust-typenum-1
  (package
    (name "rust-typenum")
    (version "1.15.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "typenum" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "11yrvz1vd43gqv738yw1v75rzngjbs7iwcgzjy3cq5ywkv2imy6w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-scale-info" ,rust-scale-info-1))))
    (home-page "https://github.com/paholg/typenum")
    (synopsis
     "Typenum is a Rust library for type-level numbers evaluated at
    compile time. It currently supports bits, unsigned integers, and signed
    integers. It also provides a type-level array of type-level numbers, but its
    implementation is incomplete.")
    (description
     "Typenum is a Rust library for type-level numbers evaluated at compile time.  It
currently supports bits, unsigned integers, and signed integers.  It also
provides a type-level array of type-level numbers, but its implementation is
incomplete.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crypto-common-0.1
  (package
    (name "rust-crypto-common")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "crypto-common" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1cvby95a6xg7kxdz5ln3rl9xh66nz66w46mm3g56ri1z5x815yqv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-typenum" ,rust-typenum-1))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis "Common cryptographic traits")
    (description "Common cryptographic traits")
    (license (list license:expat license:asl2.0))))

(define-public rust-const-oid-0.9
  (package
    (name "rust-const-oid")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "const-oid" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0q8n1zsa73130hxa2w88qw36g8nprz21j52abpva3khm59a26bkj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/RustCrypto/formats/tree/master/const-oid")
    (synopsis
     "Const-friendly implementation of the ISO/IEC Object Identifier (OID) standard
as defined in ITU X.660, with support for BER/DER encoding/decoding as well as
heapless no_std (i.e. embedded) support
")
    (description
     "Const-friendly implementation of the ISO/IEC Object Identifier (OID) standard as
defined in ITU X.660, with support for BER/DER encoding/decoding as well as
heapless no_std (i.e.  embedded) support")
    (license (list license:asl2.0 license:expat))))

(define-public rust-digest-0.10
  (package
    (name "rust-digest")
    (version "0.10.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "digest" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0v7qvhh0apbgagnj2dc1x8pnwxmvd5z4vdpjxg9cnym3cmrwbyxd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-blobby" ,rust-blobby-0.3)
                       ("rust-block-buffer" ,rust-block-buffer-0.10)
                       ("rust-const-oid" ,rust-const-oid-0.9)
                       ("rust-crypto-common" ,rust-crypto-common-0.1)
                       ("rust-subtle" ,rust-subtle-2))))
    (home-page "https://github.com/RustCrypto/traits")
    (synopsis "Traits for cryptographic hash functions")
    (description "Traits for cryptographic hash functions")
    (license (list license:expat license:asl2.0))))

(define-public rust-md-5-0.10
  (package
    (name "rust-md-5")
    (version "0.10.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "md-5" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jmrykh705dfclkgxwjysj5y8l1nyrn1gddw5xpgyjyla1l50rb3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-digest" ,rust-digest-0.10)
                       ("rust-md5-asm" ,rust-md5-asm-0.5))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "MD5 hash function")
    (description "MD5 hash function")
    (license (list license:expat license:asl2.0))))

(define-public rust-atomic-0.5
  (package
    (name "rust-atomic")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "atomic" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0k135q1qfmxxyzrlhr47r0j38r5fnd4163rgl552qxyagrk853dq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-autocfg" ,rust-autocfg-1))))
    (home-page "https://github.com/Amanieu/atomic-rs")
    (synopsis "Generic Atomic<T> wrapper type")
    (description "Generic Atomic<T> wrapper type")
    (license (list license:asl2.0 license:expat))))

(define-public rust-derive-arbitrary-1
  (package
    (name "rust-derive-arbitrary")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "derive_arbitrary" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0zw12jc6k6aixqs6m2rsj56grhx2xjw2l8rhr8rj1wj897qdy0s9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/rust-fuzz/arbitrary")
    (synopsis "Derives arbitrary traits")
    (description "Derives arbitrary traits")
    (license (list license:expat license:asl2.0))))

(define-public rust-arbitrary-1
  (package
    (name "rust-arbitrary")
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "arbitrary" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0q7xqpf9abj8yfq9632rbkdnkpig1ar3xw1hyq7rgc9q3x9j8yas"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-derive-arbitrary" ,rust-derive-arbitrary-1))))
    (home-page "https://github.com/rust-fuzz/arbitrary/")
    (synopsis
     "The trait for generating structured data from unstructured data")
    (description
     "The trait for generating structured data from unstructured data")
    (license (list license:expat license:asl2.0))))

(define-public rust-uuid-1
  (package
    (name "rust-uuid")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "uuid" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "10xyg4zzjz3m1mwhrshnx837iv8flcn6ms5hz0nvnqrkz5w1xd7y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-atomic" ,rust-atomic-0.5)
                       ("rust-getrandom" ,rust-getrandom-0.2)
                       ("rust-md-5" ,rust-md-5-0.10)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha1-smol" ,rust-sha1-smol-1)
                       ("rust-slog" ,rust-slog-2)
                       ("rust-uuid-macro-internal" ,rust-uuid-macro-internal-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-zerocopy" ,rust-zerocopy-0.6))))
    (home-page "https://github.com/uuid-rs/uuid")
    (synopsis "A library to generate and parse UUIDs.")
    (description
     "This package provides a library to generate and parse UUIDs.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-cfb-0.7
  (package
    (name "rust-cfb")
    (version "0.7.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cfb" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "03y6p3dlm7gfds19bq4ba971za16rjbn7q2v0vqcri52l2kjv3yk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-fnv" ,rust-fnv-1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/mdsteele/rust-cfb")
    (synopsis "Read/write Compound File Binary (structured storage) files")
    (description "Read/write Compound File Binary (structured storage) files")
    (license license:expat)))

(define-public rust-msi-0.5
  (package
    (name "rust-msi")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "msi" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "145cj5yb4kh8wwncxbsvn4jgqidr5s31a1jvg4axplcw3rsac8ci"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-cfb" ,rust-cfb-0.7)
                       ("rust-encoding" ,rust-encoding-0.2)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/mdsteele/rust-msi")
    (synopsis "Read/write Windows Installer (MSI) files")
    (description "Read/write Windows Installer (MSI) files")
    (license license:expat)))

(define-public rust-termcolor-1
  (package
    (name "rust-termcolor")
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "termcolor" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0mbpflskhnz3jf312k50vn0hqbql8ga2rk0k79pkgchip4q4vcms"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-winapi-util" ,rust-winapi-util-0.1))))
    (home-page "https://github.com/BurntSushi/termcolor")
    (synopsis
     "A simple cross platform library for writing colored text to a terminal.
")
    (description
     "This package provides a simple cross platform library for writing colored text
to a terminal.")
    (license (list license:unlicense license:expat))))

(define-public rust-cli-table-derive-0.4
  (package
    (name "rust-cli-table-derive")
    (version "0.4.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cli-table-derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1m4sh8z0b8q8bhxljdfl9rvk654jcdwzn93n8rn0lyv2vawvzwra"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/devashishdxt/cli-table")
    (synopsis "A crate for printing tables on command line")
    (description
     "This package provides a crate for printing tables on command line")
    (license (list license:expat license:asl2.0))))

(define-public rust-cli-table-0.4
  (package
    (name "rust-cli-table")
    (version "0.4.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cli-table" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "179pvik96qavn84rd74n3v0i4msnxq5hq39n25qbxi72v4bb3yxd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cli-table-derive" ,rust-cli-table-derive-0.4)
                       ("rust-csv" ,rust-csv-1)
                       ("rust-termcolor" ,rust-termcolor-1)
                       ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page "https://github.com/devashishdxt/cli-table")
    (synopsis "A crate for printing tables on command line")
    (description
     "This package provides a crate for printing tables on command line")
    (license (list license:expat license:asl2.0))))

(define-public rust-linux-raw-sys-0.0.46
  (package
    (name "rust-linux-raw-sys")
    (version "0.0.46")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "linux-raw-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0kc528mp2fp8m96csm6rmwg0ac7zbgf36k19ml4a4c9j6xn4blnl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/sunfishcode/linux-raw-sys")
    (synopsis "Generated bindings for Linux's userspace API")
    (description "Generated bindings for Linux's userspace API")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-async-std-1
  (package
    (name "rust-async-std")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "async-std" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pbgxhyb97h4n0451r26njvr20ywqsbm6y1wjllnp4if82s5nmk2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-attributes" ,rust-async-attributes-1)
                       ("rust-async-channel" ,rust-async-channel-1)
                       ("rust-async-global-executor" ,rust-async-global-executor-2)
                       ("rust-async-io" ,rust-async-io-1)
                       ("rust-async-lock" ,rust-async-lock-2)
                       ("rust-async-process" ,rust-async-process-1)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-io" ,rust-futures-io-0.3)
                       ("rust-futures-lite" ,rust-futures-lite-1)
                       ("rust-gloo-timers" ,rust-gloo-timers-0.2)
                       ("rust-kv-log-macro" ,rust-kv-log-macro-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pin-project-lite" ,rust-pin-project-lite-0.2)
                       ("rust-pin-utils" ,rust-pin-utils-0.1)
                       ("rust-slab" ,rust-slab-0.4)
                       ("rust-surf" ,rust-surf-2)
                       ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4))))
    (home-page "https://async.rs")
    (synopsis "Async version of the Rust standard library")
    (description "Async version of the Rust standard library")
    (license (list license:asl2.0 license:expat))))

(define-public rust-io-lifetimes-0.7
  (package
    (name "rust-io-lifetimes")
    (version "0.7.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "io-lifetimes" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1c4pqir056fiic4rl023big8xbdqic9x3l0nf8865sixpv683r76"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-async-std" ,rust-async-std-1)
                       ("rust-fs-err" ,rust-fs-err-2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-mio" ,rust-mio-0.8)
                       ("rust-os-pipe" ,rust-os-pipe-1)
                       ("rust-socket2" ,rust-socket2-0.4)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.36))))
    (home-page "https://github.com/sunfishcode/io-lifetimes")
    (synopsis "A low-level I/O ownership and borrowing library")
    (description
     "This package provides a low-level I/O ownership and borrowing library")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-errno-0.2
  (package
    (name "rust-errno")
    (version "0.2.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "errno" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18cnqgk8r6lq1n5cfy3bryiyz9zkqr10dxj49sa3fkzfamih8fgn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-errno-dragonfly" ,rust-errno-dragonfly-0.1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/lambda-fairy/rust-errno")
    (synopsis "Cross-platform interface to the `errno` variable.")
    (description "Cross-platform interface to the `errno` variable.")
    (license (list license:expat license:asl2.0))))

(define-public rust-compiler-builtins-0.1
  (package
    (name "rust-compiler-builtins")
    (version "0.1.82")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "compiler_builtins" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "00v6ywxcwxbi9hc71nlr29cavha4k1w95cs3llg4ifx7zqspdk8q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/rust-lang/compiler-builtins")
    (synopsis
     "Compiler intrinsics used by the Rust compiler. Also available for other targets
if necessary!
")
    (description
     "Compiler intrinsics used by the Rust compiler.  Also available for other targets
if necessary!")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustix-0.35
  (package
    (name "rust-rustix")
    (version "0.35.12")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustix" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1s015p72sgfb96l0dby52jsvs88aprrz68v3fb25jca2nvwlfncq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-errno" ,rust-errno-0.2)
                       ("rust-io-lifetimes" ,rust-io-lifetimes-0.7)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-linux-raw-sys" ,rust-linux-raw-sys-0.0.46)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.36))))
    (home-page "https://github.com/bytecodealliance/rustix")
    (synopsis "Safe Rust bindings to POSIX/Unix/Linux/Winsock2-like syscalls")
    (description
     "Safe Rust bindings to POSIX/Unix/Linux/Winsock2-like syscalls")
    (license (list license:asl2.0 license:asl2.0
                   license:expat))))

(define-public rust-terminal-size-0.2
  (package
    (name "rust-terminal-size")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "terminal_size" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18f57ag083ckf460wyhp34jdh193rhxrh2ja9qbgdpkrrxhchh44"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustix" ,rust-rustix-0.35)
                       ("rust-windows-sys" ,rust-windows-sys-0.36))))
    (home-page "https://github.com/eminence/terminal-size")
    (synopsis "Gets the size of your Linux or Windows terminal")
    (description "Gets the size of your Linux or Windows terminal")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-lex-0.3
  (package
    (name "rust-clap-lex")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "clap_lex" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1a4dzbnlxiamfsn0pnkhn7n9bdfjh66j9fxm6mmr7d227vvrhh8d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-os-str-bytes" ,rust-os-str-bytes-6))))
    (home-page "https://github.com/clap-rs/clap/tree/master/clap_lex")
    (synopsis "Minimal, flexible command line parser")
    (description "Minimal, flexible command line parser")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-derive-4
  (package
    (name "rust-clap-derive")
    (version "4.0.18")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "clap_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1wr8mk2shws1r10q46bhxqb94dqg681jg3n5l1fjvwra8bvb188n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-heck" ,rust-heck-0.4)
                       ("rust-proc-macro-error" ,rust-proc-macro-error-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/clap-rs/clap/tree/master/clap_derive")
    (synopsis
     "Parse command line argument by defining a struct, derive crate.")
    (description
     "Parse command line argument by defining a struct, derive crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-4
  (package
    (name "rust-clap")
    (version "4.0.18")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "clap" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0jxh5as3ax8mdw0zm465w1zajjzpia5x3rmgbwr45pnj9rv6fn1k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atty" ,rust-atty-0.2)
                       ("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-clap-derive" ,rust-clap-derive-4)
                       ("rust-clap-lex" ,rust-clap-lex-0.3)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-strsim" ,rust-strsim-0.10)
                       ("rust-termcolor" ,rust-termcolor-1)
                       ("rust-terminal-size" ,rust-terminal-size-0.2)
                       ("rust-unicase" ,rust-unicase-2)
                       ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis
     "A simple to use, efficient, and full-featured Command Line Argument Parser")
    (description
     "This package provides a simple to use, efficient, and full-featured Command Line
Argument Parser")
    (license (list license:expat license:asl2.0))))

(define-public rust-lzxd-0.1
  (package
    (name "rust-lzxd")
    (version "0.1.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "lzxd" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04xjqs0n9yrl5ifsr2bq1aqqqa2amnj3z5ny8pdxznfx1pr64i3q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/Lonami/lzxd")
    (synopsis
     "Decompression implementation for Microsoft's LZXD compression format.
")
    (description
     "Decompression implementation for Microsoft's LZXD compression format.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cab-0.4
  (package
    (name "rust-cab")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cab" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0l9mgdp8jynb8xr2v08kbb3qc74mhwnrbk6k3xiw0fbx7ki4ssxf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-lzxd" ,rust-lzxd-0.1)
                       ("rust-time" ,rust-time-0.3))))
    (home-page "https://github.com/mdsteele/rust-cab")
    (synopsis "Read/write Windows cabinet (CAB) files")
    (description "Read/write Windows cabinet (CAB) files")
    (license license:expat)))

(define-public rust-xwin-0.2
  (package
    (name "rust-xwin")
    (version "0.2.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "xwin" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "17wvzgn2ywv3n99n192zp386ppbsfq6awgxi7jknsai6riqfyriz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-cab" ,rust-cab-0.4)
                       ("rust-camino" ,rust-camino-1)
                       ("rust-clap" ,rust-clap-3)
                       ("rust-cli-table" ,rust-cli-table-0.4)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-indicatif" ,rust-indicatif-0.17)
                       ("rust-msi" ,rust-msi-0.5)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-parking-lot" ,rust-parking-lot-0.12)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
                       ("rust-twox-hash" ,rust-twox-hash-1)
                       ("rust-ureq" ,rust-ureq-2)
                       ("rust-zip" ,rust-zip-0.6))
       #:cargo-development-inputs (("rust-insta" ,rust-insta-1)
                                   ("rust-similar-asserts" ,rust-similar-asserts-1)
                                   ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/Jake-Shadle/xwin")
    (synopsis
     "Allows downloading and repacking the MSVC CRT and Windows SDK for cross compilation")
    (description
     "Allows downloading and repacking the MSVC CRT and Windows SDK for cross
compilation")
    (license (list license:asl2.0 license:expat))))

(define-public rust-libc-0.2
  (package
    (name "rust-libc")
    (version "0.2.137")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libc" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12dz2lk4a7lm03k079n2rkm1l6cpdhvy6nrngbfprzrv19icqzzw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1))))
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc.
")
    (description "Raw FFI bindings to platform libraries like libc.")
    (license (list license:expat license:asl2.0))))

(define-public rust-which-4
  (package
    (name "rust-which")
    (version "4.3.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "which" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0yybp94wikf21vkcl8b6w6l5pnd95nl4fxryz669l4lyxsxiz0qw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-either" ,rust-either-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-regex" ,rust-regex-1))))
    (home-page "https://github.com/harryfei/which-rs.git")
    (synopsis
     "A Rust equivalent of Unix command \"which\". Locate installed executable in cross platforms.")
    (description
     "This package provides a Rust equivalent of Unix command \"which\".  Locate
installed executable in cross platforms.")
    (license license:expat)))

(define-public rust-path-slash-0.2
  (package
    (name "rust-path-slash")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "path-slash" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0hjgljv4vy97qqw9gxnwzqhhpysjss2yhdphfccy3c388afhk48y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rhysd/path-slash")
    (synopsis "Conversion to/from a file path from/to slash path")
    (description "Conversion to/from a file path from/to slash path")
    (license license:expat)))

(define-public rust-vte-0.10
  (package
    (name "rust-vte")
    (version "0.10.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "vte" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "10srmy9ssircrwsb5lpx3fbhx71460j77kvz0krz38jcmf9fdg3c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.5)
                       ("rust-utf8parse" ,rust-utf8parse-0.2)
                       ("rust-vte-generate-state-changes" ,rust-vte-generate-state-changes-0.1))))
    (home-page "https://github.com/alacritty/vte")
    (synopsis "Parser for implementing terminal emulators")
    (description "Parser for implementing terminal emulators")
    (license (list license:asl2.0 license:expat))))

(define-public rust-vt100-0.15
  (package
    (name "rust-vt100")
    (version "0.15.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "vt100" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0h7mmh68fr8xxn7fw4lziz1yvs3qv1sm3wmbb228f7a1w0n32hbm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-itoa" ,rust-itoa-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-vte" ,rust-vte-0.10))))
    (home-page "https://github.com/doy/vt100-rust")
    (synopsis "Library for parsing terminal data")
    (description "Library for parsing terminal data")
    (license license:expat)))

(define-public rust-indicatif-0.17
  (package
    (name "rust-indicatif")
    (version "0.17.0-rc.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "indicatif" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1wnapd0h3kgywy1dci0gd5ikzwpsnakpbylwxvkc2y83m1m52ii9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-console" ,rust-console-0.15)
                       ("rust-number-prefix" ,rust-number-prefix-0.4)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                       ("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-vt100" ,rust-vt100-0.15))
       #:cargo-development-inputs (("rust-once-cell" ,rust-once-cell-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-structopt" ,rust-structopt-0.3)
                                   ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/console-rs/indicatif")
    (synopsis "A progress bar and cli reporting library for Rust")
    (description
     "This package provides a progress bar and cli reporting library for Rust")
    (license license:expat)))

(define-public rust-fs-err-2
  (package
    (name "rust-fs-err")
    (version "2.8.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "fs-err" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0l2wxmfsy2sngyc7szcg9cj7y3zisn46fdm68cpndw3054k3xnv4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/andrewhickman/fs-err")
    (synopsis
     "A drop-in replacement for std::fs with more helpful error messages.")
    (description
     "This package provides a drop-in replacement for std::fs with more helpful error
messages.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cargo-xwin-0.10
  (package
    (name "rust-cargo-xwin")
    (version "0.10.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cargo-xwin" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1im6mjnjszy401y2nv53w2dq74w9l8kbidkhpwy7gp9z1spl8k8b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-cargo-options" ,rust-cargo-options-0.3)
                       ("rust-clap" ,rust-clap-3)
                       ("rust-dirs" ,rust-dirs-4)
                       ("rust-fs-err" ,rust-fs-err-2)
                       ("rust-indicatif" ,rust-indicatif-0.17)
                       ("rust-path-slash" ,rust-path-slash-0.2)
                       ("rust-which" ,rust-which-4)
                       ("rust-xwin" ,rust-xwin-0.2))))
    (home-page "https://github.com/messense/cargo-xwin")
    (synopsis "Cross compile Cargo project to Windows MSVC target with ease")
    (description
     "Cross compile Cargo project to Windows MSVC target with ease")
    (license license:expat)))

(define-public rust-cargo-options-0.3
  (package
    (name "rust-cargo-options")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cargo-options" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "19mb4j5q5bc7k1482rngs8zdl20ychc0gggk4qfa716g0ays6y8i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-clap" ,rust-clap-3))))
    (home-page "https://github.com/messense/cargo-options")
    (synopsis "Reusable common Cargo command line options")
    (description "Reusable common Cargo command line options")
    (license license:expat)))

(define-public rust-maturin-0.13
  (package
    (name "rust-maturin")
    (version "0.13.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "maturin" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1rx55avhf505qx5zlxd2gns7czx127fbizndp1snzvpawcribl90"))
              (patches (list (local-file "patches/maturin-hack-out-rpath.patch")))))
    (build-system cargo-build-system)
    (arguments
     `(#:rust ,rust-1.63
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-bytesize" ,rust-bytesize-1)
                       ("rust-cargo-options" ,rust-cargo-options-0.3)
                       ("rust-cargo-xwin" ,rust-cargo-xwin-0.10)
                       ("rust-cargo-zigbuild" ,rust-cargo-zigbuild-0.12)
                       ("rust-cargo-metadata" ,rust-cargo-metadata-0.15)
                       ("rust-cargo-platform" ,rust-cargo-platform-0.1)
                       ("rust-cbindgen" ,rust-cbindgen-0.24)
                       ("rust-cc" ,rust-cc-1)
                       ("rust-clap" ,rust-clap-3)
                       ("rust-clap-complete" ,rust-clap-complete-3)
                       ("rust-clap-complete-fig" ,rust-clap-complete-fig-3)
                       ("rust-configparser" ,rust-configparser-3)
                       ("rust-console" ,rust-console-0.15)
                       ("rust-dialoguer" ,rust-dialoguer-0.10)
                       ("rust-dirs" ,rust-dirs-4)
                       ("rust-dunce" ,rust-dunce-1)
                       ("rust-fat-macho" ,rust-fat-macho-0.4)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-fs-err" ,rust-fs-err-2)
                       ("rust-glob" ,rust-glob-0.3)
                       ("rust-goblin" ,rust-goblin-0.5)
                       ("rust-human-panic" ,rust-human-panic-1)
                       ("rust-ignore" ,rust-ignore-0.4)
                       ("rust-keyring" ,rust-keyring-1)
                       ("rust-lddtree" ,rust-lddtree-0.3)
                       ("rust-minijinja" ,rust-minijinja-0.20)
                       ("rust-multipart" ,rust-multipart-0.18)
                       ("rust-native-tls" ,rust-native-tls-0.2)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pep440" ,rust-pep440-0.2)
                       ("rust-platform-info" ,rust-platform-info-1)
                       ("rust-pyproject-toml" ,rust-pyproject-toml-0.3)
                       ("rust-python-pkginfo" ,rust-python-pkginfo-0.5)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-rpassword" ,rust-rpassword-6)
                       ("rust-rustc-version" ,rust-rustc-version-0.4)
                       ("rust-semver" ,rust-semver-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-tar" ,rust-tar-0.4)
                       ("rust-target-lexicon" ,rust-target-lexicon-0.12)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-textwrap" ,rust-textwrap-0.15)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-toml-edit" ,rust-toml-edit-0.14)
                       ("rust-tracing" ,rust-tracing-0.1)
                       ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
                       ("rust-ureq" ,rust-ureq-2)
                       ("rust-zip" ,rust-zip-0.6))
       #:cargo-development-inputs (("rust-indoc" ,rust-indoc-1)
                                   ("rust-pretty-assertions" ,rust-pretty-assertions-1)
                                   ("rust-rustversion" ,rust-rustversion-1))
       #:tests? #f
       #:phases
       ,#~(modify-phases %standard-phases
            (add-after 'unpack 'hack-out-patchelf
              (lambda _
                (substitute* "src/build_context.rs"
                  (("if let Err\\(err\\) = patchelf::set_rpath.*\\{.*\\{\\}: \\{\\}.*\\}") ""))
                (substitute* "src/build_context.rs"
                  ((".*patchelf::set_soname" all) (string-append "// " all))
                  ((".*patchelf::set_rpath" all) (string-append "// " all))
                  ((".*patchelf::replace_needed" all) (string-append "// " all)))))
            (add-before 'package 'delete-orig
              (lambda _
                (delete-file "Cargo.toml.orig"))))))
    (home-page "https://github.com/pyo3/maturin")
    (synopsis
     "Build and publish crates with pyo3, rust-cpython and cffi bindings as well as rust binaries as python packages")
    (description
     "Build and publish crates with pyo3, rust-cpython and cffi bindings as well as
rust binaries as python packages")
    (license (list license:expat license:asl2.0))))

(define-public rust-cargo-platform-0.1
  (package
    (name "rust-cargo-platform")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cargo-platform" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "09zsf76b9yr02jh17xq925xp1w824w2bwvb78fd0gpx5m1fq5nyb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/rust-lang/cargo")
    (synopsis "Cargo's representation of a target platform.")
    (description "Cargo's representation of a target platform.")
    (license (list license:expat license:asl2.0))))

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

(define rust-serde-derive-1
  (package
    (name "rust-serde-derive")
    (version "1.0.147")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "serde_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ln8rqbybpxmk4fvh6lgm75acs1d8x90fi44fhx3x77wm0n3c7ag"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://serde.rs")
    (synopsis "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
     "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-1
  (package
    (name "rust-serde")
    (version "1.0.147")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "serde" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rc9jj8bbhf3lkf07ln8kyljigyzc4kk90nzg4dc2gwqmsdxd4yi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://serde.rs")
    (synopsis "A generic serialization/deserialization framework")
    (description
     "This package provides a generic serialization/deserialization framework")
    (license (list license:expat license:asl2.0))))
