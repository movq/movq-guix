;;; Modified 2022 by Mike Jones <mike@mjones.io>
;;; Based on nonguix
;;; Original copyright notice:
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Hebi Li <hebi@lihebi.com>
;;; Copyright © 2020 Malte Frank Gerdes <malte.f.gerdes@gmail.com>
;;; Copyright © 2020, 2021 Jean-Baptiste Volatier <jbv@pm.me>
;;; Copyright © 2020-2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;;
;;; This file is not part of GNU Guix.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (movq packages nvidia)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license-gnu:)
  #:use-module ((nonguix licenses) #:prefix license:)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (nongnu packages linux)
  #:use-module (movq packages glvnd)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

; Used for closed-source packages
(define nvidia-version "535.113.01")

(define-public nvidia-driver
  (package
    (name "nvidia-driver")
    (version nvidia-version)
    (source
     (origin
       (uri (format #f "http://us.download.nvidia.com/XFree86/Linux-x86_64/~a/~a.run"
                    version
                    (format #f "NVIDIA-Linux-x86_64-~a" version)))
       (sha256 (base32 "10bl2qiqy3haqzij4xfrg1as7z32ikrlnh30kxg7y6z8vzc09qr8"))
       (method url-fetch)))
    (build-system linux-module-build-system)
    (arguments
     (list #:linux linux
       #:tests? #f
       #:phases
       #~(modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys #:rest r)
             (let ((source (assoc-ref inputs "source")))
               (invoke "sh" source "--extract-only")
               (chdir #$(format #f "NVIDIA-Linux-x86_64-~a" version)))))
         (replace 'build
           (lambda*  (#:key inputs outputs #:allow-other-keys)
             ;; We cannot use with-directory-excursion, because the install
             ;; phase needs to be in the kernel folder. Otherwise no .ko
             ;; would be installed.
             (chdir "kernel")
             ;; Patch Kbuild
             (substitute* "Kbuild"
               (("/bin/sh") (string-append #$bash-minimal "/bin/sh")))
             (invoke "make"
                     "-j"
                     (string-append "SYSSRC="
                                    (assoc-ref inputs "linux-module-builder")
                                    "/lib/modules/build")
                     "CC=gcc")))
         (delete 'strip)
         (add-after 'install 'install-copy
           (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)
             (chdir "..")
             (use-modules (ice-9 ftw)
                          (ice-9 regex)
                          (ice-9 textual-ports))
             (let* ((libdir (string-append #$output "/lib"))
                    (bindir (string-append #$output "/bin"))
                    (etcdir (string-append #$output "/etc")))
                           ;; Add udev rules for nvidia
               (let ((rulesdir (string-append #$output "/lib/udev/rules.d/"))
                     (rules    (string-append #$output "/lib/udev/rules.d/90-nvidia.rules"))
                     (sh       (string-append #$bash-minimal "/bin/sh"))
                     (mknod    (string-append #$coreutils "/bin/mknod"))
                     (cut     (string-append #$coreutils "/bin/cut"))
                     (grep     (string-append #$grep "/bin/grep")))
                 (mkdir-p rulesdir)
                 (call-with-output-file rules
                   (lambda (port)
                     (put-string port
                                 (string-append
                                  "KERNEL==\"nvidia\", "
                                  "RUN+=\"" sh " -c '" mknod " -m 666 /dev/nvidiactl c $$(" grep " nvidia-frontend /proc/devices | " cut " -d \\  -f 1) 255'\"" "\n"
                                  "KERNEL==\"nvidia_modeset\", "
                                  "RUN+=\"" sh " -c '" mknod " -m 666 /dev/nvidia-modeset c $$(" grep " nvidia-frontend /proc/devices | " cut " -d \\  -f 1) 254'\"" "\n"
                                  "KERNEL==\"card*\", SUBSYSTEM==\"drm\", DRIVERS==\"nvidia\", "
                                  "RUN+=\"" sh " -c '" mknod " -m 666 /dev/nvidia0 c $$(" grep " nvidia-frontend /proc/devices | " cut " -d \\  -f 1) 0'\"" "\n"
                                  "KERNEL==\"nvidia_uvm\", "
                                  "RUN+=\"" sh " -c '" mknod " -m 666 /dev/nvidia-uvm c $$(" grep " nvidia-uvm /proc/devices | " cut " -d \\  -f 1) 0'\"" "\n"
                                  "KERNEL==\"nvidia_uvm\", "
                                  "RUN+=\"" sh " -c '" mknod " -m 666 /dev/nvidia-uvm-tools c $$(" grep " nvidia-uvm /proc/devices | " cut " -d \\  -f 1) 0'\"" "\n" )))))
              ))))))
    (supported-systems '("x86_64-linux"))
    (native-inputs
     (list
       patchelf
       perl
       python-2
       which
       xz
       zstd))
    (inputs
     (list
       atk
       bash-minimal
       cairo
       coreutils
       `(,gcc "lib")
       gdk-pixbuf
       glib
       grep
       gtk+
       gtk+-2
       kmod
       glibc
       libdrm
       libx11
       libxext
       linux
       mesa
       pango
       wayland))
    (home-page "https://www.nvidia.com")
    (synopsis "Proprietary Nvidia driver")
    (description "This is the evil Nvidia driver.  Don't forget to add
nvidia-driver to the udev-rules in your config.scm:
@code{(simple-service 'custom-udev-rules udev-service-type (list nvidia-driver))}
Further xorg should be configured by adding:
@code{(modules (cons* nvidia-driver %default-xorg-modules))
(drivers '(\"nvidia\"))} to @code{xorg-configuration}.")
    (license (license:nonfree (format #f "file:///share/doc/nvidia-driver-~a/LICENSE" version)))))

(define-public nvidia-libs
  (package
    (name "nvidia-libs")
    (version nvidia-version)
    (source
     (origin
       (uri (format #f "http://us.download.nvidia.com/XFree86/Linux-x86_64/~a/~a.run"
                    version
                    (format #f "NVIDIA-Linux-x86_64-~a" version)))
       (sha256 (base32 "10bl2qiqy3haqzij4xfrg1as7z32ikrlnh30kxg7y6z8vzc09qr8"))
       (method url-fetch)))
    (build-system copy-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda* (#:key inputs #:allow-other-keys #:rest r)
                   (let ((source (assoc-ref inputs "source")))
                     (invoke "sh" source "--extract-only")
                     (chdir #$(format #f "NVIDIA-Linux-x86_64-~a" version))
                     #t)))
               (delete 'build)
               (delete 'strip)
               (delete 'check)
               (add-after 'install 'patch-symlink
                 (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)
                   (use-modules (ice-9 ftw)
                                (ice-9 regex)
                                (ice-9 textual-ports))
                   (let* ((out #$output)
                          (libdir (string-append out "/lib"))
                          (bindir (string-append out "/bin"))
                          (etcdir (string-append out "/etc")))
                     ;; ------------------------------
                     ;; patchelf
                     (let* ((ld.so (string-append #$glibc #$(glibc-dynamic-linker)))
                            (rpath (string-join
                                    (list "$ORIGIN"
                                          (string-append out "/lib")
                                          (string-append #$glibc "/lib")
                                          (string-append #$atk "/lib")
                                          (string-append #$cairo "/lib")
                                          (string-append #$gcc:lib "/lib")
                                          (string-append #$gdk-pixbuf "/lib")
                                          (string-append #$glib "/lib")
                                          (string-append #$gtk+ "/lib")
                                          (string-append #$gtk+-2 "/lib")
                                          (string-append #$libdrm "/lib")
                                          (string-append #$libx11 "/lib")
                                          (string-append #$libxcb "/lib")
                                          (string-append #$libxext "/lib")
                                          (string-append #$mesa "/lib")
                                          (string-append #$pango "/lib")
                                          (string-append #$wayland "/lib"))
                                    ":")))
                       (define (patch-elf file)
                         (format #t "Patching ~a ...~%" file)
                         (unless (string-contains file ".so")
                           (invoke "patchelf" "--set-interpreter" ld.so file))
                         (invoke "patchelf" "--set-rpath" rpath file))
                       (for-each (lambda (file)
                                   (when (elf-file? file)
                                     (patch-elf file)))
                                 (find-files out  ".*\\.so"))
                       (when (string=? "x86_64-linux" #$(%current-system))
                         (patch-elf (string-append #$output "/bin/nvidia-smi"))))

                     ;; ------------------------------
                     ;; Create short name symbolic links
                     (for-each (lambda (file)
                                 (when (not (string-contains file "libglxserver_nvidia"))
                                   (let* ((short (regexp-substitute
                                                  #f
                                                  (string-match "([^/]*\\.so).*" file)
                                                  1))
                                          (major (cond
                                                  ((or (string=? short "libGLX_nvidia.so")
                                                       (string=? short "libEGL_nvidia.so")) "0")
                                                  (else "1")))
                                          (mid (string-append short "." major))
                                          (short-file (string-append libdir "/" short))
                                          (mid-file (string-append libdir "/" mid)))
                                     ;; FIXME the same name, print out warning at least
                                     ;; [X] libEGL.so.1.1.0
                                     ;; [ ] libEGL.so.435.21
                                     (when (not (file-exists? short-file))
                                       (format #t "Linking ~a to ~a ...~%" short file)
                                       (symlink (basename file) short-file))
                                     (when (not (file-exists? mid-file))
                                       (format #t "Linking ~a to ~a ...~%" mid file)
                                       (symlink (basename file) mid-file)))))
                               (find-files libdir "\\.so\\."))
                                        ; Fix JSON files
                     (for-each (lambda (file)
                                 (substitute* file
                                   (("libGLX_nvidia\\.so\\.0") (string-append libdir "/libGLX_nvidia.so.0"))
                                   (("libEGL_nvidia\\.so\\.0") (string-append libdir "/libEGL_nvidia.so.0"))
                                   (("libnvidia-egl-wayland\\.so\\.1") (string-append libdir "/libnvidia-egl-wayland.so.1"))
                                   (("libnvidia-egl-gbm\\.so\\.1") (string-append libdir "/libnvidia-egl-gbm\\.so\\.1"))))
                               (list (string-append out "/share/vulkan/icd.d/nvidia_icd.json")
                                     (string-append out "/share/vulkan/implicit_layer.d/nvidia_layers.json")
                                     (string-append out "/share/glvnd/egl_vendor.d/10_nvidia.json")
                                     (string-append out "/share/egl/egl_external_platform.d/10_nvidia_wayland.json")
                                     (string-append out "/share/egl/egl_external_platform.d/15_nvidia_gbm.json")))
                     (when (string=? "x86_64-linux" #$(%current-system))
                       (symlink (string-append "libglxserver_nvidia.so."
                                               #$(package-version nvidia-driver))
                                (string-append #$output "/lib/xorg/modules/extensions/" "libglxserver_nvidia.so"))
                       (mkdir (string-append #$output "/lib/gbm"))
                       (symlink (string-append #$output "/lib/libnvidia-allocator.so." #$(package-version nvidia-driver))
                                (string-append #$output "/lib/gbm/nvidia-drm_gbm.so")))

                     #t))))
           #:install-plan
           (match (%current-system)
             ("x86_64-linux" #~'(("." "lib" #:include-regexp ("^./[^/]+\\.so")
                                  #:exclude ("nvidia_drv.so"
                                             "libEGL.so.1.1.0"
                                             #$(string-append "libEGL.so." (package-version nvidia-driver))
                                             "libGLESv1_CM.so.1.2.0"
                                             "libGLESv2.so.2.1.0"
                                             "libGL.so.1.7.0"
                                             "libGLX.so.0")
                                  #:exclude-regexp ("libglxserver_nvidia.so.*"))
                                 ("." "lib/xorg/modules/extensions" #:include-regexp ("libglxserver_nvidia\\.so\\.*"))
                                 ("nvidia_drv.so" "lib/xorg/modules/drivers/")
                                 ("nvidia-smi" "bin/")
                                 ("nvidia_icd.json" "share/vulkan/icd.d/")
                                 ("nvidia_layers.json" "share/vulkan/implicit_layer.d/")
                                 ("10_nvidia.json" "share/glvnd/egl_vendor.d/")
                                 ("10_nvidia_wayland.json" "share/egl/egl_external_platform.d/")
                                 ("15_nvidia_gbm.json" "share/egl/egl_external_platform.d/")))
             ("i686-linux" #~'(("32" "lib" #:include-regexp ("^./[^/]+\\.so"))
                               ("nvidia_icd.json" "share/vulkan/icd.d/")
                               ("nvidia_layers.json" "share/vulkan/implicit_layer.d/")
                               ("10_nvidia.json" "share/glvnd/egl_vendor.d/")
                               ("10_nvidia_wayland.json" "share/egl/egl_external_platform.d/")
                               ("15_nvidia_gbm.json" "share/egl/egl_external_platform.d/")))
             (_ #~'()))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (native-inputs (list patchelf perl python-2 which xz zstd))
    (inputs (list atk
                  cairo
                  gdk-pixbuf
                  glib
                  gtk+
                  gtk+-2
                  glibc
                  libdrm
                  libx11
                  libxext
                  libxcb
                  mesa
                  wayland))
    (home-page "https://www.nvidia.com")
    (synopsis "Libraries of the proprietary Nvidia driver")
    (description "These are the libraries of the evil Nvidia driver compatible
with the ones usually provided by Mesa.  To use these libraries with
packages that have been compiled with a mesa output, take a look at the nvda
package.")
    (license (license:nonfree (format #f "file:///share/doc/nvidia-driver-~a/LICENSE" version)))))

(define-public nvidia-settings
  (package
    (name "nvidia-settings")
    (version nvidia-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/NVIDIA/nvidia-settings")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00q8mxvmx4sg937lqvgmqck83vmm3gnbhr3k3q52f6ndpsszgr1m"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ;no test suite
           #:make-flags
           #~(list (string-append "PREFIX=" #$output)
                   (string-append "CC=" #$(cc-for-target)))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-after 'install 'wrap-program
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     (wrap-program (string-append out "/bin/nvidia-settings")
                                   `("LD_LIBRARY_PATH" ":" prefix
                                     (,(string-append out "/lib/"))))))))))
    (native-inputs (list m4
                         pkg-config))
    (inputs (list bash-minimal
                  dbus
                  glu
                  gtk+
                  gtk+-2
                  libvdpau
                  libx11
                  libxext
                  libxrandr
                  libxv
                  libxxf86vm))
    (synopsis "Nvidia driver control panel")
    (description
     "This package provides Nvidia driver control panel for monitor
configuration, creating application profiles, gpu monitoring and more.")
    (home-page "https://github.com/NVIDIA/nvidia-settings")
    (license license-gnu:gpl2)))

;; nvda is used as a name because it has the same length as mesa which is
;; required for grafting
(define-public nvda
  (package
    (inherit nvidia-libs)
    (name "nvda")
    (version "535113")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list #:modules '((guix build union))
       #:builder #~(begin
                   (use-modules (guix build union)
                                (srfi srfi-1)
                                (ice-9 regex))
                      (union-build (assoc-ref %outputs "out")
                                   (list #$mesa #$nvidia-libs)
                                   #:resolve-collision (lambda (files) (let ((file
                                                                         (if (string-match "nvidia-libs" (first files))
                                                                             (first files)
                                                                             (last files))))
                                                                         (format #t "chosen ~a ~%" file)
                                                                         file))))))
    (description "These are the libraries of the evil Nvidia driver,
packaged in such a way that you can use the transformation option
@code{--with-graft=mesa=nvda} to use the nvidia driver with a package that requires mesa.")
    (inputs
     (list mesa
           nvidia-libs))
    (outputs '("out"))))

(define mesa/fake
  (package
    (inherit mesa)
    (replacement nvda)))

(define-public replace-mesa
  (package-input-rewriting `((,mesa . ,mesa/fake))))

(define-public cuda-11.3
  (package
    (name "cuda")
    (version "11.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://developer.download.nvidia.com/compute/cuda/"
             version "/local_installers/cuda_" version "_465.19.01_linux.run"))
       (sha256 (base32 "0d19pwcqin76scbw1s5kgj8n0z1p4v1hyfldqmamilyfxycfm4xd"))))
    (build-system copy-build-system)
    (arguments
     (list #:strip-binaries? #f ; for some reason this breaks cicc
           #:validate-runpath? #f
           #:install-plan
           #~'(("cuda_cudart/targets/x86_64-linux/include" "include")
               ("cuda_cudart/targets/x86_64-linux/lib" "lib")
               ("cuda_cuobjdump/bin" "bin")
               ("cuda_cupti/extras/CUPTI/lib64" "lib")
               ("cuda_cupti/extras/CUPTI/include" "include")
               ("cuda_cuxxfilt/bin" "bin")
               ("cuda_nvcc/bin" "bin")
               ("cuda_nvcc/nvvm/bin" "nvvm/bin")
               ("cuda_nvcc/nvvm/lib64" "nvvm/lib")
               ("cuda_nvcc/nvvm/libdevice" "nvvm/libdevice")
               ("cuda_nvcc/nvvm/include" "nvvm/include")
               ("cuda_nvcc/targets/x86_64-linux/include" "include")
               ("cuda_nvcc/targets/x86_64-linux/lib" "lib")
               ("cuda_nvprof/bin" "bin")
               ("cuda_nvprof/targets/x86_64-linux/include" "include")
               ("cuda_nvprof/targets/x86_64-linux/lib" "lib")
               ("cuda_nvtx/targets/x86_64-linux/include" "include")
               ("cuda_nvtx/targets/x86_64-linux/lib" "lib")
               ("cuda_thrust/targets/x86_64-linux/include" "include"))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda _
                   (invoke "sh" #$source "--keep" "--noexec")
                   (chdir "pkg/builds")))
               (add-after 'install 'patch-elf
                 (lambda _
                   (let ((ld.so (string-append #$glibc #$(glibc-dynamic-linker)))
                         (rpath (string-join
                                 (list "$ORIGIN"
                                       (string-append #$output "/lib")
                                       (string-append #$glibc "/lib")
                                       (string-append #$gcc:lib "/lib"))
                                 ":")))
                     (define (patch-elf file)
                       (unless (string-contains file ".so")
                         (format #t "Setting interpreter of ~a to ~a~%" file ld.so)
                         (invoke "patchelf" "--set-interpreter" ld.so file))
                       (format #t "Setting RPATH to: ~a~%" rpath)
                       (invoke "patchelf" "--set-rpath" rpath file))
                     (for-each (lambda (file)
                                 (when (elf-file? file)
                                   (patch-elf file)))
                               (find-files #$output))))))))
    (native-inputs (list patchelf))
    (home-page #f)
    (description #f)
    (synopsis #f)
    (license #f)))

(define-public nv-codec-headers
  (package
    (name "nv-codec-headers")
    (version "11.1.5.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://git.videolan.org/git/ffmpeg/nv-codec-headers.git")
               (commit (string-append "n" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "05a6dfv6yackcyx7ysxfzf2h768y63ih8icdkkzi8bwq7hp8lcy9"))))
    (build-system gnu-build-system)
    (arguments
      (list #:tests? #f
            #:make-flags
            #~(list (string-append "PREFIX=" #$output))
            #:phases
            #~(modify-phases %standard-phases
                (add-before 'build 'set-dlopen-paths
                  (lambda _
                    (substitute* "include/ffnvcodec/dynlink_loader.h"
                      (("libcuda\\.so\\.1" all) (string-append #$nvidia-libs "/lib/" all))
                      (("libnvcuvid\\.so\\.1" all) (string-append #$nvidia-libs "/lib/" all))
                      (("libnvidia-encode\\.so\\.1" all) (string-append #$nvidia-libs "/lib/" all)))))
                (delete 'configure))))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))

(define-public nvidia-vaapi-driver
  (package
    (name "nvidia-vaapi-driver")
    (version "0.0.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/elFarto/nvidia-vaapi-driver")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0gpsf35dlfiw14zzrws1mp8n343z3zmhvvwjxl6251il7fmc5lzz"))))
    (build-system meson-build-system)
    (arguments
      (list #:phases
            #~(modify-phases %standard-phases
                (add-before 'configure 'link-pthread
                  (lambda _
                    (substitute* "meson.build"
                      (("^libva_deps = .*\n" all) (string-append all "thread_dep = dependency('threads')\n"))
                      (("dl_dep,\n" all) (string-append all "thread_dep\n"))))))))
    (inputs (list libglvnd-guix gst-plugins-bad nv-codec-headers libva nvidia-libs))
    (native-inputs (list pkg-config))
    (native-search-paths
     (list (search-path-specification
            (variable "LIBVA_DRIVERS_PATH")
            (files '("lib/dri")))))
    (home-page #f)
    (description #f)
    (synopsis #f)
    (license #f)))
