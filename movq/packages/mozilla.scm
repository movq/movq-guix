;;; Adapted from nonguix by Mike Jones <mike@mjones.io>
;;; 2022-08-17
;;; Original copyright header

;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018 ng0 <gillmann@infotropique.org>
;;; Copyright © 2017, 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2020, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2019, 2020 Adrian Malacoda <malacoda@monarch-pass.net>
;;; Copyright © 2020-2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2021 pineapples <guixuser6392@protonmail.com>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021, 2022 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Pierre Langlois <pierre.langlois@gmx.com>
;;;
;;; This file is not part of GNU Guix.
;;;
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

(define-module (movq packages mozilla)
  #:use-module (gnu packages llvm)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (nongnu packages mozilla))

; Firefox with some tweaks:
;  * Enable link-time optimisation
;  * Use -march=native -O3 CFLAGS
;  * Use latest rust and llvm
;  * Disable WASM stuff
(define-public firefox-movq
  (package
    (inherit firefox)
    (arguments
      (substitute-keyword-arguments (package-arguments firefox)
        ((#:configure-flags flags)
          #~(let ((clang #$(this-package-native-input "clang")))
            `("--enable-application=browser"
              "--without-wasm-sandboxed-libraries"

              ;; Configuration
              "--with-system-jpeg"
              "--with-system-zlib"
              ;; "--with-system-png" ;require libpng-apng >= 1.6.35
              "--with-system-icu"
              "--enable-system-ffi"
              "--enable-system-pixman"
              "--enable-jemalloc"

              ;; see https://bugs.gnu.org/32833
              "--with-system-nspr"
              ;; "--with-system-nss"

              ,(string-append "--with-clang-path="
                              clang "/bin/clang")
              ,(string-append "--with-libclang-path="
                              clang "/lib")

              ;; Distribution
              "--with-distribution-id=org.nonguix"
              "--disable-official-branding"

              ;; Features
              "--disable-tests"
              "--disable-updater"
              "--enable-pulseaudio"
              "--disable-crashreporter"

              ;; Build details
              "--disable-debug"
              "--enable-rust-simd"
              "--enable-release"
              "--enable-optimize"
              "--enable-strip"
              "--disable-elf-hack"
              "--enable-linker=lld"
              "--enable-lto=cross")))
        ((#:phases phases)
           #~(modify-phases #$phases
             (replace 'configure
               (lambda* (#:key inputs outputs configure-flags #:allow-other-keys)
                 (setenv "AUTOCONF" (string-append (assoc-ref inputs "autoconf")
                                                   "/bin/autoconf"))
                 (setenv "SHELL" (which "bash"))
                 (setenv "CONFIG_SHELL" (which "bash"))
                 (setenv "MACH_BUILD_PYTHON_NATIVE_PACKAGE_SOURCE" "system")
                 ;; This should use the host info probably (does firefox build on
                 ;; non-x86_64 though?)
                 (setenv "GUIX_PYTHONPATH"
                         (string-append (getcwd)
                                        "/obj-x86_64-pc-linux-gnu/_virtualenvs/build"))

                 ;; Use Clang, Clang is 2x faster than GCC
                 (setenv "AR" "llvm-ar")
                 (setenv "NM" "llvm-nm")
                 (setenv "CC" "clang")
                 (setenv "CXX" "clang++")
                 (setenv "CFLAGS" "-march=sandybridge -O3")
                 (setenv "CXXFLAGS" "-march=sandybridge -O3")
                 (setenv "RUSTFLAGS" "-C opt-level=3 -C target-cpu=sandybridge")

                 (setenv "MOZ_NOSPAM" "1")

                 (setenv "MOZBUILD_STATE_PATH" (getcwd))

                 (let* ((mozconfig (string-append (getcwd) "/mozconfig"))
                        (out (assoc-ref outputs "out"))
                        (flags (cons (string-append "--prefix=" out)
                                     configure-flags)))
                   (format #t "build directory: ~s~%" (getcwd))
                   (format #t "configure flags: ~s~%" flags)

                   (define write-flags
                     (lambda flags
                       (display (string-join
                                 (map (cut string-append "ac_add_options " <>)
                                      flags)
                                 "\n"))
                       (display "\n")))
                   (with-output-to-file mozconfig
                     (lambda ()
                       (apply write-flags flags)
                       ;; The following option unsets Telemetry Reporting. With the Addons Fiasco,
                       ;; Mozilla was found to be collecting user's data, including saved passwords and
                       ;; web form data, without users consent. Mozilla was also found shipping updates
                       ;; to systems without the user's knowledge or permission.
                       ;; As a result of this, use the following command to permanently disable
                       ;; telemetry reporting in Firefox.
                       (display "unset MOZ_TELEMETRY_REPORTING\n")))
                   (setenv "MOZCONFIG" mozconfig))
                 (invoke "./mach" "configure")))))))
    (native-inputs
      (modify-inputs (package-native-inputs firefox)
        (prepend lld-wrapper)
        (delete "wasm32-wasi-clang-toolchain")))))
