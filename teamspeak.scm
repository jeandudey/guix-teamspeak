;;; SPDX-FileCopyrightText: © 2024 Jean-Pierre De Jesus DIAZ <me@jeandudey.tech>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (teamspeak)
  #:use-module (gnu packages)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (nonguix licenses))

(define-public teamspeak
  (package
    (name "teamspeak")
    (version "3.13.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.teamspeak-services.com"
                                  "/releases/server/" version
                                  "/teamspeak3-server_linux_amd64-" version
                                  ".tar.bz2"))
              (sha256
               (base32
                "07h4a06p987j9499q4skhlv1najnpkcnq1prr3j03640m4qmfnkp"))))
    (build-system binary-build-system)
    (arguments
     (list #:patchelf-plan
           #~'(("ts3server" ("gcc" "glibc"))
               ("tsdns/tsdnsserver" ("glibc"))
               ("libts3_ssh.so" ("glibc"))
               ("libts3db_postgresql.so" ("gcc" "glibc" "postgresql"))
               ("libts3db_sqlite3.so" ("gcc")))

           #:imported-modules `(,@%binary-build-system-modules
                                ,@%gnu-build-system-modules)

           #:modules '(((guix build gnu-build-system) #:prefix gnu:)
                       (guix build utils)
                       (ice-9 popen)
                       (ice-9 textual-ports)
                       (nonguix build binary-build-system))

           ;; - LICENSE file is installed by installed-license-files phase. 
           ;;
           ;; - libts3db_mariadb.so can't be used as there's no package in
           ;; GNU Guix for libmariadb.so.2, only libmariadb.so.3.
           ;;
           ;; - The scripts are not usefult because they try to change
           ;; directory to the store to execute the teamspeak server which
           ;; will fail because the file system is read-only.
           #:install-plan
           #~'(("." #$(string-append "share/teamspeak-" version "/")
               #:exclude ("3RD_PARTY_LICENSES" "CHANGELOG" "LICENSE"
                          "libts3db_mariadb.so"
                          "ts3server_minimal_runscript.sh"
                          "ts3server_startscript.sh"))
               ("3RD_PARTY_LICENSES"
                #$(string-append "share/doc/teamspeak-" version
                                 "/3RD_PARTY_LICENSES"))
               ("CHANGELOG"
                #$(string-append "share/doc/teamspeak-" version
                                 "/CHANGELOG"))
               ("doc" #$(string-append "share/doc/teamspeak-" version)))

           #:phases
           #~(modify-phases %standard-phases
               (add-after 'patchelf 'patchelf-ts3server-extra
                 (lambda _
                   (let* ((arguments `("patchelf" "--print-rpath" "ts3server"))
                          (command (string-join arguments))
                          (port (open-input-pipe command))
                          (raw-rpath (get-string-all port))
                          (old-rpath (string-delete #\newline raw-rpath))
                          (new-rpath (string-append old-rpath ":" #$output
                                                    "/share/teamspeak-"
                                                    #$version)))
                     (close-pipe port)

                     (format #t "Setting RUNPATH of \"ts3server\" to ~s.~%" new-rpath)
                     (invoke "patchelf" "--set-rpath" new-rpath "ts3server"))))
               (add-after 'install 'delete-not-excluded
                 (lambda _
                   (delete-file-recursively
                     (string-append #$output "/share/teamspeak-"
                                    #$version "/doc"))
                   (delete-file-recursively
                     (string-append #$output "/share/teamspeak-"
                                    #$version "/redist"))))
               (add-after 'delete-not-excluded 'create-symbolic-links
                 (lambda _
                   (let ((bin (string-append #$output "/bin"))
                         (teamspeak (string-append #$output "/share/teamspeak-" #$version)))
                     (mkdir-p bin)
                     (symlink (string-append teamspeak "/ts3server")
                              (string-append bin "/ts3server"))
                     (symlink (string-append teamspeak "/tsdns/tsdnsserver")
                              (string-append bin "/tsdnsserver")))))
               (replace 'validate-runpath
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((validate-runpath
                           (assoc-ref gnu:%standard-phases 'validate-runpath)))
                     (validate-runpath #:outputs outputs
                                       #:elf-directories
                                       '("bin" "share"))))))))
    (inputs
     (list `(,gcc "lib")
           glibc
           postgresql))
    (home-page "https://www.teamspeak.com/")
    (synopsis "Server for proprietary voice chat software")
    (description "This package provides the TeamSpeak Server for linux.")
    (license (nonfree "file://LICENSE"))))
