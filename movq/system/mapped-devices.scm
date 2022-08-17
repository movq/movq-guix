;;; Adapted from GNU Guix
;;; modified 2022-08-17 by Mike Jones <mike@mjones.io>
;;;
;;; Copyright © 2014-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017, 2018 Mark H Weaver <mhw@netris.org>
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

(define-module (movq system mapped-devices)
  #:use-module (gnu build file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system uuid)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (ice-9 match)
  #:autoload   (gnu packages cryptsetup) (cryptsetup-static)
  #:export (luks-device-mapping-discard))

(define (open-luks-device-discard source targets)
  "Return a gexp that maps SOURCE to TARGET as a LUKS device, using
'cryptsetup'."
  (with-imported-modules (source-module-closure
                          '((gnu build file-systems)
                            (guix build utils))) ;; For mkdir-p
    (match targets
      ((target)
       #~(let ((source #$(if (uuid? source)
                             (uuid-bytevector source)
                             source)))
           ;; XXX: 'use-modules' should be at the top level.
           (use-modules (rnrs bytevectors) ;bytevector?
                        ((gnu build file-systems)
                         #:select (find-partition-by-luks-uuid
                                   system*/tty))
                        ((guix build utils) #:select (mkdir-p)))

           ;; Create '/run/cryptsetup/' if it does not exist, as device locking
           ;; is mandatory for LUKS2.
           (mkdir-p "/run/cryptsetup/")

           ;; Use 'cryptsetup-static', not 'cryptsetup', to avoid pulling the
           ;; whole world inside the initrd (for when we're in an initrd).
           ;; 'cryptsetup open' requires standard input to be a tty to allow
           ;; for interaction but shepherd sets standard input to /dev/null;
           ;; thus, explicitly request a tty.
           (zero? (system*/tty
                   #$(file-append cryptsetup-static "/sbin/cryptsetup")
                   "open" "--type" "luks" "--allow-discards"

                   ;; Note: We cannot use the "UUID=source" syntax here
                   ;; because 'cryptsetup' implements it by searching the
                   ;; udev-populated /dev/disk/by-id directory but udev may
                   ;; be unavailable at the time we run this.
                   (if (bytevector? source)
                       (or (let loop ((tries-left 10))
                             (and (positive? tries-left)
                                  (or (find-partition-by-luks-uuid source)
                                      ;; If the underlying partition is
                                      ;; not found, try again after
                                      ;; waiting a second, up to ten
                                      ;; times.  FIXME: This should be
                                      ;; dealt with in a more robust way.
                                      (begin (sleep 1)
                                             (loop (- tries-left 1))))))
                           (error "LUKS partition not found" source))
                       source)

                   #$target)))))))

(define (close-luks-device source targets)
  "Return a gexp that closes TARGET, a LUKS device."
  (match targets
    ((target)
     #~(zero? (system* #$(file-append cryptsetup-static "/sbin/cryptsetup")
                       "close" #$target)))))

(define* (check-luks-device md #:key
                            needed-for-boot?
                            (initrd-modules '())
                            #:allow-other-keys
                            #:rest rest)
  "Ensure the source of MD is valid."
  (let ((source   (mapped-device-source md))
        (location (mapped-device-location md)))
    (or (not (zero? (getuid)))
        (if (uuid? source)
            (match (find-partition-by-luks-uuid (uuid-bytevector source))
              (#f
               (raise (make-compound-condition
                       (formatted-message (G_ "no LUKS partition with UUID '~a'")
                                          (uuid->string source))
                       (condition
                        (&error-location
                         (location (source-properties->location
                                    (mapped-device-location md))))))))
              ((? string? device)
               (check-device-initrd-modules device initrd-modules location)))
            (check-device-initrd-modules source initrd-modules location)))))

(define luks-device-mapping-discard
  ;; The type of LUKS mapped devices.
  (mapped-device-kind
   (open open-luks-device-discard)
   (close close-luks-device)
   (check check-luks-device)))
