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
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define rust-bootstrapped-package
  (@@ (gnu packages rust) rust-bootstrapped-package))

(define rust-1.60
  (@@ (gnu packages rust) rust-1.60))

(define rust-1.61
  (rust-bootstrapped-package
   rust-1.60 "1.61.0" "1vfs05hkf9ilk19b2vahqn8l6k17pl9nc1ky9kgspaascx8l62xd"))

(define-public rust-1.62
  (rust-bootstrapped-package
   rust-1.61 "1.62.1" "0gqkg34ic77dcvsz69qbdng6g3zfhl6hnhx7ha1mjkyrzipvxb3j"))

(define-public rust-1.63
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.62 "1.63.0"
          "1l4rrbzhxv88pnfq94nbyb9m6lfnjwixma3mwjkmvvs2aqlq158z")))
    (package
      (inherit base-rust)
      (outputs (cons "rustfmt" (package-outputs base-rust)))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'build
               ;; Phase overridden to also build rustfmt.
               (lambda* (#:key parallel-build? #:allow-other-keys)
                 (let ((job-spec (string-append
                                  "-j" (if parallel-build?
                                           (number->string (parallel-job-count))
                                           "1"))))
                   (invoke "./x.py" job-spec "build"
                           "library/std" ;rustc
                           "src/tools/cargo"
                           "src/tools/rustfmt"))))
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
                 (invoke "./x.py" "install" "rustfmt"))))))))))

(define-public rust-current rust-1.63)
