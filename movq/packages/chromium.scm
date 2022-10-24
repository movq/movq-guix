(define-module (movq packages chromium)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages python)
  )

(define %chromium-version (@@ (gnu packages chromium) %chromium-version))
(define %preserved-third-party-files
  (append (list "third_party/devtools-frontend/src/test/unittests/front_end/third_party/i18n"
                "third_party/maldoca"
                "third_party/maldoca/src/third_party/tensorflow_protos"
                "third_party/maldoca/src/third_party/zlibwrapper"
                "third_party/lzma_sdk"
                "third_party/unrar")
          (@@ (gnu packages chromium) %preserved-third-party-files)))
(define %debian-patches (@@ (gnu packages chromium) %debian-patches))
(define %gcc-patches (@@ (gnu packages chromium) %gcc-patches))
(define %guix-patches (@@ (gnu packages chromium) %guix-patches))
(define %reverse-patches (@@ (gnu packages chromium) %reverse-patches))

(define googled-chromium-snippet
  ;; Note: delay to cope with cyclic module imports at the top level.
  (delay
    #~(begin
        (let ((chromium-dir (getcwd)))
          (set-path-environment-variable
           "PATH" '("bin")
           (list #+patch #+python-wrapper #+xz))

          ;; Apply patches before running the ungoogled scripts because
          ;; domain substitution may break some of the patches.
          (format #t "Applying assorted build fixes...~%")
          (force-output)
          (for-each (lambda (patch)
                      (invoke "patch" "-p1" "--force" "--input"
                              patch "--no-backup-if-mismatch"))
                    (append '#+%debian-patches '#+%guix-patches
                            '#+%gcc-patches))

          ;; These patches are "reversed", i.e. their changes should be undone.
          (for-each (lambda (patch)
                      (invoke "patch" "-Rp1" "-F3" "--force" "--input"
                              patch "--no-backup-if-mismatch"))
                    '#+%reverse-patches)

          (format #t "Pruning third party files...~%")
          (force-output)
          (apply invoke "python"
                 "build/linux/unbundle/remove_bundled_libraries.py"
                 "--do-remove" '#$%preserved-third-party-files)

          (format #t "Replacing GN files...~%")
          (force-output)
          (substitute* "tools/generate_shim_headers/generate_shim_headers.py"
            ;; The "is_official_build" configure option enables certain
            ;; release optimizations like those used in the commercial
            ;; Chrome browser.  Unfortunately it also requires using the
            ;; bundled libraries: lose that restriction.
            (("#if defined\\(OFFICIAL_BUILD\\)")
             "#if 0"))
          (invoke "python" "build/linux/unbundle/replace_gn_files.py"
                  "--system-libraries" "ffmpeg" "flac" "fontconfig"
                  "freetype" "harfbuzz-ng" "icu" "libdrm" "libevent"
                  "libjpeg" "libpng" "libwebp" "libxml" "libxslt"
                  "openh264" "opus" "re2" "zlib")))))

(define-public googled-chromium
  (package/inherit ungoogled-chromium
  (name "googled-chromium")
  (source (origin
            (method url-fetch)
            (uri (string-append "https://commondatastorage.googleapis.com"
                                "/chromium-browser-official/chromium-"
                                %chromium-version ".tar.xz"))
            (sha256
              (base32
               "14niglj8q6mfkmgbbjhaipmyhv6vryx93crswb1xa871a14in28g"))
            (modules '((guix build utils)))
            (snippet (force googled-chromium-snippet))))
  (arguments
   (substitute-keyword-arguments (package-arguments ungoogled-chromium)
     ((#:configure-flags flags #f)
      #~(append (list "enable_hangout_services_extension=true")
                (delete "safe_browsing_mode=0"
                 (delete "build_with_tflite_lib=false" #$flags))))
     ((#:phases phases #f)
      #~(modify-phases #$phases
          (add-after 'unpack 'fix-eu-strip
            (lambda _
              (invoke "mkdir" "-p" "buildtools/third_party/eu-strip/bin")
              (invoke "ln" "-s" (string-append #$coreutils "/bin/true") "buildtools/third_party/eu-strip/bin/eu-strip")))
          (add-before 'install 'rename-chromedriver
            (lambda _
              (rename-file "out/Release/chromedriver.unstripped" "out/Release/chromedriver")))))))))
