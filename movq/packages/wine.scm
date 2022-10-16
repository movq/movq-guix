;;; Modified by Mike Jones <mike@mjones.io> 2022.
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017, 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2022 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;;
;;; This file is part of GNU Guix.
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

(define-module (movq packages wine)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mingw)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages scanner)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages wine)
  #:use-module (gnu packages xorg)
  #:use-module (movq packages glvnd)
  #:use-module (ice-9 match)
  )

(define-public lutris-wine
  (replace-mesa (package
    (name "lutris-wine")
    (version "7.2-2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lutris/wine")
             (commit (string-append "lutris" "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h7k3dhs6q1jc5ba67s0spd16r636bg2x04nz7dqvrxjawv7ncdw"))))
    (build-system gnu-build-system)
    (native-inputs
     (list bison flex gettext-minimal perl pkg-config))
    (inputs
     ;; Some libraries like libjpeg are now compiled into native PE objects.
     ;; The ELF objects provided by Guix packages are of no use.  Whilst this
     ;; is technically bundling, it's quite defensible.  It might be possible
     ;; to build some of these from Guix PACKAGE-SOURCE but attempts were not
     ;; fruitful so far.  See <https://www.winehq.org/announce/7.0>.
     (list alsa-lib
           cups
           dbus
           eudev
           faudio
           fontconfig
           freetype
           gnutls
           gst-plugins-base
           libgphoto2
           openldap
           samba
           sane-backends
           libpcap
           libusb
           libice
           libx11
           libxi
           libxext
           libxcursor
           libxrender
           libxrandr
           libxinerama
           libxxf86vm
           libxcomposite
           mit-krb5
           openal
           pulseaudio
           sdl2
           unixodbc
           v4l-utils
           vkd3d
           vulkan-loader))
    (arguments
     (list
      ;; Force a 32-bit build targeting a similar architecture, i.e.:
      ;; armhf for armhf/aarch64, i686 for i686/x86_64.
      #:system (match (%current-system)
                 ((or "armhf-linux" "aarch64-linux") "armhf-linux")
                 (_ "i686-linux"))

      ;; XXX: There's a test suite, but it's unclear whether it's supposed to
      ;; pass.
      #:tests? #f

      #:configure-flags
      #~(list (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib/wine32"))

      #:make-flags
      #~(list "SHELL=bash"
              (string-append "libdir=" #$output "/lib/wine32"))

      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-SHELL
            (lambda _
              (substitute* "configure"
                ;; configure first respects CONFIG_SHELL, clobbers SHELL later.
                (("/bin/sh")
                 (which "bash")))))
          (add-after 'configure 'patch-dlopen-paths
            ;; Hardcode dlopened sonames to absolute paths.
            (lambda _
              (let* ((library-path (search-path-as-string->list
                                    (getenv "LIBRARY_PATH")))
                     (find-so (lambda (soname)
                                (search-path library-path soname))))
                (substitute* "include/config.h"
                  (("(#define SONAME_.* )\"(.*)\"" _ defso soname)
                   (format #f "~a\"~a\"" defso (find-so soname)))))))
          (add-after 'patch-generated-file-shebangs 'patch-makedep
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* "tools/makedep.c"
                (("output_filenames\\( unix_libs \\);" all)
                 (string-append all
                                "output ( \" -Wl,-rpath=%s \", so_dir );"))))))))
    (home-page "https://www.winehq.org/")
    (synopsis "Implementation of the Windows API (32-bit only)")
    (description
     "Wine (originally an acronym for \"Wine Is Not an Emulator\") is a
compatibility layer capable of running Windows applications.  Instead of
simulating internal Windows logic like a virtual machine or emulator, Wine
translates Windows API calls into POSIX calls on-the-fly, eliminating the
performance and memory penalties of other methods and allowing you to cleanly
integrate Windows applications into your desktop.")
    ;; Any platform should be able to build wine, but based on '#:system' these
    ;; are thr ones we currently support.
    (supported-systems '("i686-linux" "x86_64-linux" "armhf-linux"))
    (license license:lgpl2.1+))))

(define-public lutris-wine64
  (replace-mesa (package
    (inherit lutris-wine)
    (name "lutris-wine64")
    (inputs (modify-inputs (package-inputs lutris-wine)
              (prepend lutris-wine)))
    (arguments
     (cons*
      #:make-flags
      #~(list "SHELL=bash"
              (string-append "libdir=" #$output "/lib/wine64"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-SHELL
            (lambda _
              (substitute* "configure"
                ;; configure first respects CONFIG_SHELL, clobbers SHELL later.
                (("/bin/sh")
                 (which "bash")))))
          (add-after 'patch-generated-file-shebangs 'patch-makedep
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* "tools/makedep.c"
                (("output_filenames\\( unix_libs \\);" all)
                 (string-append all
                                "output ( \" -Wl,-rpath=%s \", so_dir );")))))
          (add-after 'install 'copy-wine32-binaries
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref %outputs "out")))
                ;; Copy the 32-bit binaries needed for WoW64.
                (copy-file (search-input-file inputs "/bin/wine")
                           (string-append out "/bin/wine"))
                (copy-file (search-input-file inputs "/bin/wine-preloader")
                           (string-append out "/bin/wine-preloader")))))
          (add-after 'install 'copy-wine32-libraries
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref %outputs "out")))
                (copy-recursively (search-input-directory inputs "/lib/wine32")
                                  (string-append out "/lib/wine32")))))
          (add-after 'compress-documentation 'copy-wine32-manpage
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref %outputs "out")))
                ;; Copy the missing man file for the wine binary from wine.
                (copy-file (search-input-file inputs "/share/man/man1/wine.1.gz")
                           (string-append out "/share/man/man1/wine.1.gz")))))
          (add-after 'configure 'patch-dlopen-paths
            ;; Hardcode dlopened sonames to absolute paths.
            (lambda _
              (let* ((library-path (search-path-as-string->list
                                    (getenv "LIBRARY_PATH")))
                     (find-so (lambda (soname)
                                (search-path library-path soname))))
                (substitute* "include/config.h"
                  (("(#define SONAME_.* )\"(.*)\"" _ defso soname)
                   (format #f "~a\"~a\"" defso (find-so soname))))))))
      #:configure-flags
      #~(list "--enable-win64"
              (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib/wine64"))
      (strip-keyword-arguments '(#:configure-flags #:make-flags #:phases
                                 #:system)
                               (package-arguments wine))))
    (synopsis "Implementation of the Windows API (WoW64 version)")
    (supported-systems '("x86_64-linux" "aarch64-linux")))))

(define mingw-toolchain-i686
  (package
    (name "mingw-toolchain-i686")
    (source #f)
    (version "10.3.0") ;; XXX: (package-version gcc) doesn't work for some reason
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union))
       #:builder (begin
                   (use-modules (guix build union))
                   (union-build (assoc-ref %outputs "out")
                                (map (lambda (x) (cdr x)) %build-inputs)))))
    (inputs
     (let* ((triplet "i686-w64-mingw32")
            (xgcc-base (cross-gcc triplet
                                  #:xbinutils (cross-binutils triplet)
                                  #:libc mingw-w64-i686-winpthreads))
            (xgcc (package
                    (inherit xgcc-base)
                    (arguments
                     (substitute-keyword-arguments (package-arguments xgcc-base)
                       ((#:configure-flags flags)
                        `(append '("--enable-threads=posix")
                                 ,flags)))))))
       (list xgcc
             (cross-binutils triplet))))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))

(define mingw-toolchain-x86_64
  (package
    (name "mingw-toolchain-x86_64")
    (source #f)
    (version "10.3.0") ;; XXX: (package-version gcc) doesn't work for some reason
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union))
       #:builder (begin
                   (use-modules (guix build union))
                   (union-build (assoc-ref %outputs "out")
                                (map (lambda (x) (cdr x)) %build-inputs)))))
    (inputs
     (let* ((triplet "x86_64-w64-mingw32")
            (xgcc-base (cross-gcc triplet
                                  #:xbinutils (cross-binutils triplet)
                                  #:libc mingw-w64-x86_64-winpthreads))
            (xgcc (package
                    (inherit xgcc-base)
                    (arguments
                     (substitute-keyword-arguments (package-arguments xgcc-base)
                       ((#:configure-flags flags)
                        `(append '("--enable-threads=posix")
                                 ,flags)))))))
       (list xgcc
             (cross-binutils triplet))))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))

(define dxvk32
  ;; This package provides 32-bit dxvk libraries on 64-bit systems.
  (package
    (name "dxvk32")
    (version "1.10.3")
    (home-page "https://github.com/doitsujin/dxvk/")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qcjfcij6qx372ac1h382bg9spf2b6nfk8zzmimnl93kbk5dkpag"))))
    (build-system meson-build-system)
    (arguments
     (list #:system "i686-linux"
           #:configure-flags '(list "--cross-file"
                                    (string-append (assoc-ref %build-inputs "source")
                                                   "/build-win32.txt"))
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'setenv
                 (lambda* (#:key inputs #:allow-other-keys)
                   (setenv "CROSS_LIBRARY_PATH"
                           (string-join (list (string-append #$mingw-w64-i686-winpthreads "/lib"))
                                        ":"))
                   (setenv "CROSS_C_INCLUDE_PATH"
                           (string-join (list (string-append #$mingw-w64-i686-winpthreads "/include"))
                                        ":"))
                   (setenv "CROSS_CPLUS_INCLUDE_PATH"
                           (string-join (list
                                         (string-append #$mingw-toolchain-i686 "/include/c++")
                                         (string-append #$mingw-w64-i686-winpthreads "/include"))
                                        ":"))))
               (replace 'strip
                 (lambda _
                   (for-each (lambda (file)
                               (invoke "i686-w64-mingw32-strip" file))
                             (find-files (string-append #$output) "\\.dll$")))))))
    (native-inputs
     (list glslang
           mingw-toolchain-i686))
    (inputs
     (list mingw-w64-i686-winpthreads wine))
    (synopsis "Vulkan-based D3D9, D3D10 and D3D11 implementation for Wine")
    (description "A Vulkan-based translation layer for Direct3D 9/10/11 which
allows running complex 3D applications with high performance using Wine.

Use @command{setup_dxvk} to install the required libraries to a Wine prefix.")
    (supported-systems '("x86_64-linux"))
    (license license:zlib)))

(define-public dxvk
  (package
    (inherit dxvk32)
    (name "dxvk")
    (arguments
     (list #:configure-flags #~(list "--cross-file"
                                   (string-append (assoc-ref %build-inputs "source")
                                                  "/build-win"
                                                  #$(match (%current-system)
                                                     ("x86_64-linux" "64")
                                                     (_ "32"))
                                                  ".txt"))
       #:phases
       #~(modify-phases %standard-phases
           #$@(if (string=? (%current-system) "x86_64-linux")
                  `((add-after 'unpack 'install-32
                      (lambda* (#:key inputs outputs #:allow-other-keys)
                        (let* ((out (assoc-ref outputs "out"))
                               (dxvk32 (assoc-ref inputs "dxvk32")))
                          (mkdir-p (string-append out "/lib32"))
                          (copy-recursively (string-append dxvk32 "/bin")
                                            (string-append out "/lib32"))
                          #t))))
                  '())
           (add-after 'install 'install-setup
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin/setup_dxvk")))
                 (mkdir-p (string-append out "/bin"))
                 (copy-file "../source/setup_dxvk.sh"
                            bin)
                 (chmod bin #o755)
                 (substitute* bin
                   (("x32") #$(match (%current-system)
                               ("x86_64-linux" "../lib32")
                               (_ "../lib")))
                   (("x64") "../lib")))))
           (add-after 'install 'move-dlls
             (lambda _
               (with-directory-excursion (string-append #$output "/bin")
                 (for-each (lambda (file)
                             (rename-file file (string-append "../lib/" file)))
                           (find-files "." "\\.dll$")))))
           (add-after 'install 'delete-static-libs
             (lambda _
               (for-each (lambda (file)
                           (delete-file file))
                         (find-files #$output "\\.a$"))))
           (add-before 'configure 'setenv
                 (lambda* (#:key inputs #:allow-other-keys)
                   (setenv "CROSS_LIBRARY_PATH"
                           (string-join (list (string-append #$mingw-w64-x86_64-winpthreads "/lib"))
                                        ":"))
                   (setenv "CROSS_C_INCLUDE_PATH"
                           (string-join (list (string-append #$mingw-w64-x86_64-winpthreads "/include"))
                                        ":"))
                   (setenv "CROSS_CPLUS_INCLUDE_PATH"
                           (string-join (list
                                         (string-append #$mingw-toolchain-x86_64 "/include/c++")
                                         (string-append #$mingw-w64-x86_64-winpthreads "/include"))
                                        ":"))))
           (replace 'strip
             (lambda _
               (for-each (lambda (file)
                           (make-file-writable file)
                           (invoke "x86_64-w64-mingw32-strip" file))
                         (find-files (string-append #$output) "\\.dll$")))))))
    (inputs
     `(("wine" ,(match (%current-system)
                  ("x86_64-linux" wine64-staging)
                  (_ wine)))
       ,@(match (%current-system)
           ("x86_64-linux"
            `(("dxvk32" ,dxvk32)))
           (_ '()))
       ("mingw-w64-x86_64" ,mingw-w64-x86_64)))
    (native-inputs (list mingw-toolchain-x86_64 glslang))
    (supported-systems '("i686-linux" "x86_64-linux"))))

(define vkd3d-proton32
  (package
    (name "vkd3d-proton32")
    (version "2.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/HansKristian-Work/vkd3d-proton")
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256 (base32 "05q2fla04ylq2825rlzr63ipjrkdmac20vm64wcczi033bvbrkj1"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--cross-file" (string-append (assoc-ref %build-inputs "source") "/build-win32.txt"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-git-version
                 (lambda _
                   (substitute* "meson.build"
                     (("command : .*abbrev.*$") "command : ['echo', '3e5aab6fb3e18f8'],"))))
               (add-before 'configure 'setenv
                 (lambda _
                   (setenv "CROSS_LIBRARY_PATH"
                           (string-join (list (string-append #$mingw-w64-i686-winpthreads "/lib"))
                                        ":"))
                   (setenv "CROSS_C_INCLUDE_PATH"
                           (string-join (list (string-append #$mingw-w64-i686-winpthreads "/include"))
                                        ":"))
                   (setenv "CROSS_CPLUS_INCLUDE_PATH"
                           (string-join (list
                                         (string-append #$mingw-toolchain-i686 "/include/c++")
                                         (string-append #$mingw-w64-i686-winpthreads "/include"))
                                        ":"))))
               (add-after 'install 'move-dlls
                 (lambda _
                   (with-directory-excursion (string-append #$output "/bin")
                     (for-each (lambda (file)
                                 (rename-file file (string-append "../lib/" file)))
                               (find-files "." "\\.dll$")))))
               (replace 'strip
                 (lambda _
                   (for-each (lambda (file)
                               (make-file-writable file)
                               (invoke "i686-w64-mingw32-strip" file))
                             (find-files (string-append #$output) "\\.dll$")))))))
    (native-inputs (list mingw-toolchain-i686 glslang wine-staging))
    (inputs (list mingw-w64-i686))
    (home-page "https://github.com/HansKristian-Work/vkd3d-proton")
    (synopsis #f)
    (description #f)
    (license #f)))

(define-public vkd3d-proton
  (package
    (inherit vkd3d-proton32)
    (name "vkd3d-proton")
    (arguments
     (list #:configure-flags
           #~(list "--cross-file" (string-append (assoc-ref %build-inputs "source") "/build-win64.txt"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-git-version
                 (lambda _
                   (substitute* "meson.build"
                     (("command : .*abbrev.*$") "command : ['echo', '3e5aab6fb3e18f8'],"))))
               (add-before 'configure 'setenv
                 (lambda _
                   (setenv "CROSS_LIBRARY_PATH"
                           (string-join (list (string-append #$mingw-w64-x86_64-winpthreads "/lib"))
                                        ":"))
                   (setenv "CROSS_C_INCLUDE_PATH"
                           (string-join (list (string-append #$mingw-w64-x86_64-winpthreads "/include"))
                                        ":"))
                   (setenv "CROSS_CPLUS_INCLUDE_PATH"
                           (string-join (list
                                         (string-append #$mingw-toolchain-x86_64 "/include/c++")
                                         (string-append #$mingw-w64-x86_64-winpthreads "/include"))
                                        ":"))))
               (add-after 'install 'move-dlls
                 (lambda _
                   (with-directory-excursion (string-append #$output "/bin")
                     (for-each (lambda (file)
                                 (rename-file file (string-append "../lib/" file)))
                               (find-files "." "\\.dll$")))))
               (add-after 'install 'install-setup
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (bin (string-append out "/bin/setup_vkd3d_proton")))
                     (mkdir-p (string-append out "/bin"))
                     (copy-file "../source/setup_vkd3d_proton.sh"
                                bin)
                     (chmod bin #o755)
                     (substitute* bin
                       (("x86") #$(match (%current-system)
                                    ("x86_64-linux" "../lib32")
                                    (_ "../lib")))
                       (("x64") "../lib")))))
               (add-after 'unpack 'install-32
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (vkd3d-proton32 (assoc-ref inputs "vkd3d-proton32")))
                     (mkdir-p (string-append out "/lib32"))
                     (copy-recursively (string-append vkd3d-proton32 "/lib")
                                       (string-append out "/lib32"))
                     #t)))
               (replace 'strip
                 (lambda _
                   (for-each (lambda (file)
                               (make-file-writable file)
                               (invoke "x86_64-w64-mingw32-strip" file))
                             (find-files (string-append #$output) "\\.dll$")))))))
    (native-inputs (list mingw-toolchain-x86_64 glslang wine64-staging vkd3d-proton32))
    (inputs (list mingw-w64-x86_64))
    (home-page "https://github.com/HansKristian-Work/vkd3d-proton")
    (synopsis #f)
    (description #f)
    (license #f)))
