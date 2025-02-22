(define-module (movq packages llvm)
  #:use-module (guix build utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages gcc)
)

(define %llvm-release-monitoring-url
  "https://github.com/llvm/llvm-project/releases")

(define %llvm-monorepo-hashes
  '(("14.0.6" . "14f8nlvnmdkp9a9a79wv67jbmafvabczhah8rwnqrgd5g3hfxxxx")
    ("15.0.3" . "1bxbirgr7hbksmw696g62ny04hyc4f4h44srjr39k23d4s4jhm7y")))

(define %llvm-patches
  '(("14.0.6" . ("clang-14.0-libc-search-path.patch"))
    ("15.0.3" . ("clang-14.0-libc-search-path.patch"))))

(define (llvm-monorepo version)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/llvm/llvm-project")
          (commit (string-append "llvmorg-" version))))
    (file-name (git-file-name "llvm-project" version))
    (sha256 (base32 (assoc-ref %llvm-monorepo-hashes version)))
    (patches (map search-patch (assoc-ref %llvm-patches version)))))

(define* (clang-runtime-from-llvm llvm
                                  #:optional
                                  hash
                                  (patches '()))
  (package
    (name "clang-runtime")
    (version (package-version llvm))
    (source
     (if hash
         (origin
           (method url-fetch)
           (uri (llvm-uri "compiler-rt" version))
           (sha256 (base32 hash))
           (patches (map search-patch patches)))
         (llvm-monorepo (package-version llvm))))
    (build-system cmake-build-system)
    (native-inputs (package-native-inputs llvm))
    (inputs
     (list llvm))
    (arguments
     `(;; Don't use '-g' during the build to save space.
       #:build-type "Release"
       #:tests? #f                      ; Tests require gtest
       #:modules ((srfi srfi-1)
                  (ice-9 match)
                  ,@%cmake-build-system-modules)
       #:phases (modify-phases (@ (guix build cmake-build-system) %standard-phases)
                  (add-after 'set-paths 'hide-glibc
                    ;; Work around https://issues.guix.info/issue/36882.  We need to
                    ;; remove glibc from CPLUS_INCLUDE_PATH so that the one hardcoded
                    ;; in GCC, at the bottom of GCC include search-path is used.
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let* ((filters '("libc"))
                             (input-directories
                              (filter-map (lambda (input)
                                            (match input
                                              ((name . dir)
                                               (and (not (member name filters))
                                                    dir))))
                                          inputs)))
                        (set-path-environment-variable "CPLUS_INCLUDE_PATH"
                                                       '("include")
                                                       input-directories)
                        #t))))))
    (home-page "https://compiler-rt.llvm.org")
    (synopsis "Runtime library for Clang/LLVM")
    (description
     "The \"clang-runtime\" library provides the implementations of run-time
functions for C and C++ programs.  It also provides header files that allow C
and C++ source code to interface with the \"sanitization\" passes of the clang
compiler.  In LLVM this library is called \"compiler-rt\".")
    (license (package-license llvm))
    (properties `((release-monitoring-url . ,%llvm-release-monitoring-url)
                  (upstream-name . "compiler-rt")))
    ;; <https://compiler-rt.llvm.org/> doesn't list MIPS as supported.
    (supported-systems (delete "mips64el-linux" %supported-systems))))

(define-public llvm-15
  (package
    (inherit llvm-14)
    (version "15.0.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/llvm/llvm-project.git")
               (commit (string-append "llvmorg-" version))))
        (file-name (git-file-name "llvm-project" version))
        (sha256 (base32 "1bxbirgr7hbksmw696g62ny04hyc4f4h44srjr39k23d4s4jhm7y"))))))

(define-public clang-runtime-15
  (let ((template (clang-runtime-from-llvm llvm-15)))
    (package
      (inherit template)
      (arguments
       (substitute-keyword-arguments (package-arguments template)
         ((#:phases phases '(@ (guix build cmake-build-system) %standard-phases))
          #~(modify-phases #$phases
              (add-after 'unpack 'change-directory
                (lambda _
                  (chdir "compiler-rt")))))))
      (native-inputs
       `(;; FIXME: libfuzzer fails to build with GCC 10.
         ("gcc" ,gcc-11)
         ,@(package-native-inputs template))))))
