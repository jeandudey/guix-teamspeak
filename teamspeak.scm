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

(define-public teamspeak-server
  (package
    (name "teamspeak-server")
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
                       (ice-9 format)
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
           #~'(("." #$(string-append "share/teamspeak-server-" version "/")
               #:exclude ("3RD_PARTY_LICENSES" "CHANGELOG" "LICENSE"
                          "libts3db_mariadb.so"
                          "ts3server_minimal_runscript.sh"
                          "ts3server_startscript.sh"))
               ("3RD_PARTY_LICENSES"
                #$(string-append "share/doc/teamspeak-server-" version
                                 "/3RD_PARTY_LICENSES"))
               ("CHANGELOG"
                #$(string-append "share/doc/teamspeak-server-" version
                                 "/CHANGELOG"))
               ("doc" #$(string-append "share/doc/teamspeak-server-" version)))

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
                                                    "/share/teamspeak-server-"
                                                    #$version)))
                     (close-pipe port)

                     (format #t "Setting RUNPATH of \"ts3server\" to ~s.~%" new-rpath)
                     (invoke "patchelf" "--set-rpath" new-rpath "ts3server"))))
               (add-after 'install 'delete-not-excluded
                 (lambda _
                   (delete-file-recursively
                     (string-append #$output "/share/teamspeak-server-"
                                    #$version "/doc"))
                   (delete-file-recursively
                     (string-append #$output "/share/teamspeak-server-"
                                    #$version "/redist"))))
               (add-after 'delete-not-excluded 'create-symbolic-links
                 (lambda _
                   (let ((bin (string-append #$output "/bin"))
                         (teamspeak (string-append #$output "/share/teamspeak-server-" #$version)))
                     (mkdir-p bin)
                     (symlink (string-append teamspeak "/tsdns/tsdnsserver")
                              (string-append bin "/tsdnsserver")))))
               ;; Wrap program to set dbsqlpath and serverquerydocs_path
               ;; arguments since the binary expects it on the current working
               ;; directory, but we expect users of the package to call it
               ;; from the most convenient location.
               (add-after 'create-symbolic-links 'wrap-program
                 (lambda _
                   (let ((sh (which "bash"))
                         (ts3server (string-append
                                      #$output "/share/teamspeak-server-" #$version
                                      "/ts3server"))
                         (ts3server-wrapped (string-append #$output "/bin/ts3server")))
                     (call-with-output-file ts3server-wrapped
                       (lambda (port)
                         (format port
                                 "#!~a~%exec -a \"$0\" \"~a\" \"$@\"~%"
                                 sh
                                 ts3server
                                 (string-append "serverquerydocs_path="
                                                #$output
                                                "/share/teamspeak-server-"
                                                #$version
                                                "/serverquerydocs"))))
                     (chmod ts3server-wrapped #o755))))
               (replace 'validate-runpath
                 (lambda* (#:key outputs #:allow-other-keys)
                   ((assoc-ref gnu:%standard-phases 'validate-runpath)
                    #:outputs outputs
                    #:elf-directories '("share")))))))
    (inputs
     (list `(,gcc "lib")
           glibc
           postgresql))
    (home-page "https://www.teamspeak.com/")
    (synopsis "Server for proprietary voice chat software")
    (description "This package provides the TeamSpeak Server for linux.")
    (license (nonfree "file://LICENSE"))))
