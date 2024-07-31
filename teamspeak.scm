;;; SPDX-FileCopyrightText: © 2024 Jean-Pierre De Jesus DIAZ <me@jeandudey.tech>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (teamspeak)
  #:use-module (gnu packages)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (nonguix licenses))

;;; NOTE: Also needs swiftshader, so consider adding it to GNU Guix.
(define-public teamspeak-client
  (package
    (name "teamspeak-client")
    (version "5.0.0-beta77")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append "https://files.teamspeak-services.com"
                                  "/pre_releases/client/" version
                                  "/teamspeak-client.tar.gz"))
              (sha256
               (base32
                "06qqp678cp1bsdxa1jnp4sl0hrw38n748369xyf7jg0d29xzjfvg"))))
    (build-system chromium-binary-build-system)
    (arguments
     (list #:wrapper-plan
           #~'("TeamSpeak"
               "libcef.so"
               "libEGL.so"
               "libGLESv2.so"
               "libolm.so.3"
               "libtschat_client_lib.so"
               "libtschat_client_lib_export.so")

           #:install-plan
           #~'(("." #$(string-append "share/teamspeak-client-" version "/")))

           #:modules '((guix build utils)
                       (ice-9 format)
                       (ice-9 popen)
                       (ice-9 textual-ports)
                       (nonguix build chromium-binary-build-system))

           #:phases
           #~(modify-phases %standard-phases
               (add-after 'patchelf 'patchelf-extra
                 (lambda _
                   (let* ((arguments `("patchelf" "--print-rpath" "TeamSpeak"))
                          (command (string-join arguments))
                          (port (open-input-pipe command))
                          (raw-rpath (get-string-all port))
                          (old-rpath (string-delete #\newline raw-rpath))
                          (new-rpath (string-append old-rpath ":" #$output
                                                    "/share/teamspeak-client-"
                                                    #$version)))
                     (close-pipe port)

                     (format #t "Setting RUNPATH of \"TeamSpeak\" to ~s.~%" new-rpath)
                     (invoke "patchelf" "--set-rpath" new-rpath "TeamSpeak"))))
               (add-before 'install-wrapper 'create-symbolic-links
                 (lambda _
                   (mkdir-p (string-append #$output "/bin"))
                   (symlink (string-append #$output "/share/teamspeak-client-" #$version "/TeamSpeak")
                            (string-append #$output "/bin/TeamSpeak")))))))
    (inputs
     (list gdk-pixbuf
           harfbuzz
           libxscrnsaver
           vulkan-loader))
    (home-page "https://www.teamspeak.com")
    (synopsis "Client for proprietary voice chat software")
    (description "This package provides the TeamSpeak Client for linux.")
    (license (nonfree ""))))

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
           #~'(("ts3server" ("gcc" "glibc" "libmaxminddb"))
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
           libmaxminddb
           postgresql))
    (home-page "https://www.teamspeak.com/")
    (synopsis "Server for proprietary voice chat software")
    (description "This package provides the TeamSpeak Server for linux.")
    (license (nonfree "file://LICENSE"))))
