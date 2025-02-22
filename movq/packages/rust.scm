;;; Based on GNU Guix (gnu/packages/rust.scm)
;;; Modified 2022-08-17 by Mike Jones <mike@mjones.io>

;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 Eric Le Bihan <eric.le.bihan.dev@free.fr>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2018 Nikolai Merinov <nikolai.merinov@member.fsf.org>
;;; Copyright © 2017, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2020, 2021 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020 Matthew James Kraai <kraai@ftbfs.org>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 (unmatched parenthesis <paren@disroot.org>
;;; Copyright © 2022 Zheng Junjie <873216071@qq.com>

;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (movq packages rust)
  #:use-module (gnu packages)
  #:use-module (gnu packages llvm)
  #:use-module (guix build utils)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define rust-bootstrapped-package
  (@@ (gnu packages rust) rust-bootstrapped-package))

(define rust-1.65
  (@@ (gnu packages rust) rust-1.65))

(define-public rust-1.66
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.65 "1.66.1" "1fjr94gsicsxd2ypz4zm8aad1zdbiccr7qjfbmq8f8f7jhx96g2v")))
    (package
      (inherit base-rust)
      (inputs
        (modify-inputs (package-inputs base-rust)
          (replace "llvm" llvm-15))))))


(define-public rust-1.67
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.66 "1.67.1"
          "0vpzv6rm3w1wbni17ryvcw83k5klhghklylfdza3nnp8blz3sj26")))
    (package
      (inherit base-rust)
      (outputs (cons* "clippy" "rustfmt" (package-outputs base-rust)))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'patch-cargo-checksums
               (lambda* _
                 (substitute* '("Cargo.lock"
                                "src/bootstrap/Cargo.lock"
                                "src/tools/rust-analyzer/Cargo.lock")
                   (("(checksum = )\".*\"" all name)
                    (string-append name "\"" ,(@@ (gnu packages rust) %cargo-reference-hash) "\"")))
                 (generate-all-checksums "vendor")))
             (replace 'build
               ;; Phase overridden to also build rustfmt.
               (lambda* (#:key outputs parallel-build? #:allow-other-keys)
                 (let ((job-spec (string-append
                                  "-j" (if parallel-build?
                                           (number->string (parallel-job-count))
                                           "1"))))
                   (setenv "RUSTFLAGS" (string-append "-C link-arg=-Wl,-rpath=" (assoc-ref outputs "out") "/lib"))
                   (invoke "./x.py" job-spec "build"
                           "library/std" ;rustc
                           "src/tools/cargo"
                           "src/tools/rustfmt"
                           "src/tools/clippy"))))
             (replace 'install
               ;; Phase overridden to also install rustfmt.
               (lambda* (#:key outputs #:allow-other-keys)
                 (invoke "./x.py" "install")
                 (substitute* "config.toml"
                   ;; Adjust the prefix to the 'cargo' output.
                   (("prefix = \"[^\"]*\"")
                    (format #f "prefix = ~s" (assoc-ref outputs "cargo"))))
                 (invoke "./x.py" "install" "cargo")
                 (substitute* "config.toml"
                   ;; Adjust the prefix to the 'rustfmt' output.
                   (("prefix = \"[^\"]*\"")
                    (format #f "prefix = ~s" (assoc-ref outputs "rustfmt"))))
                 (invoke "./x.py" "install" "rustfmt")
                 (substitute* "config.toml"
                   ;; Adjust the prefix to the 'clippy output.
                   (("prefix = \"[^\"]*\"")
                    (format #f "prefix = ~s" (assoc-ref outputs "clippy"))))
                 (invoke "./x.py" "install" "clippy"))))))))))

(define-public rust-current rust-1.67)

(define-public rust-current-src
  (hidden-package
   (package
     (inherit rust-current)
     (name "rust-src")
     (build-system copy-build-system)
     (native-inputs '())
     (inputs '())
     (native-search-paths '())
     (outputs '("out"))
     (arguments
      `(#:install-plan
        '(("library" "lib/rustlib/src/rust/library")
          ("src" "lib/rustlib/src/rust/src"))))
     (synopsis "Source code for the Rust standard library")
     (description "This package provide source code for the Rust standard
library, only use by rust-analyzer, make rust-analyzer out of the box."))))
