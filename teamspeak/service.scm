;;; SPDX-FileCopyrightText: © 2024 Jean-Pierre De Jesus DIAZ <me@jeandudey.tech>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (teamspeak service)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (teamspeak)
  #:export (teamspeak-configuration
            teamspeak-configuration?
            teamspeak-configuration-package
            teamspeak-service-type))

;;;
;;; Commentary: TeamSpeak server service.
;;;
;;; This provides a service for TeamSpeak server intended to reduce
;;; maintenance efforts, upgrading should be as simple as updating the package
;;; definition.
;;;
;;; To do:
;;;
;;; - Use least-authority-wrapper to make sure that TeamSpeak server doesn't
;;; have access to the rest of the system to minimize attack surface and
;;; increase user privacy, e.g. let desktop users run the server without
;;; worrying on "what if" TeamSpeak was attacked or it purposefully tried to
;;; invade privacy.
;;;
;;; - Using Tor to communicate with the outer world, the TeamSpeak server
;;; needs to communicate back home for registration purposes.
;;;
;;; - If the user disables Tor for whatever reason, also add netfilter table
;;; rules (nftables) to only reach TeamSpeak servers. See:
;;;
;;; <https://support.teamspeak.com/hc/en-us/articles/360002712257-Which-ports-does-the-TeamSpeak-3-server-use>.
;;;
;;; - Allow using PostgreSQL.
;;;
;;; - Allow using Maxmind databases for GeoIP lookups.

(define-configuration teamspeak-configuration
  (package
    (file-like teamspeak-server)
    "The TeamSpeak server package to use"
    empty-serializer))

(define %teamspeak-accounts
  (list (user-group
          (name "teamspeak")
          (system? #t))
        (user-account
         (name "teamspeak")
         (group "teamspeak")
         (system? #t)
         (comment "TeamSpeak server daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (teamspeak-activation config)
  (match-record config <teamspeak-configuration>
    (package)
    (with-imported-modules (source-module-closure '((gnu build activation)))
     #~(begin
         (use-modules (gnu build activation))

         (let ((user (getpwnam "teamspeak")))
           (mkdir-p/perms "/var/crash/teamspeak" user #o755)
           (mkdir-p/perms "/var/run/teamspeak" user #o755)
           (mkdir-p/perms "/var/lib/teamspeak" user #o755)
           (mkdir-p/perms "/var/log/teamspeak" user #o755)

           (when (file-exists? "/var/lib/teamspeak/sql")
             (delete-file-recursively "/var/lib/teamspeak/sql"))

           (symlink #$(file-append package "/share/teamspeak-server-"
                                   (package-version package) "/sql")
                    "/var/lib/teamspeak/sql"))))))

(define (teamspeak-config-file config)
  (mixed-text-file "teamspeak.ini"
                   ;; By running this service in GNU Guix you are
                   ;; accepting the terms of the TeamSpeak license,
                   ;; so, read it.
                   "license_accepted=1\n"
                   "daemon=1\n"
                   "crashdumps_path=/var/crash/teamspeak\n"
                   "pid_file=/var/run/teamspeak/teamspeak.pid\n"
                   "logpath=/var/log/teamspeak\n"
                   "logappend=1\n"
                   "query_ssh_port=20022\n"
                   "query_ssh_ip=0.0.0.0,0::0\n"))

(define (teamspeak-shepherd-service config)
  (match-record config <teamspeak-configuration>
    (package)
    (let ((teamspeak.ini (teamspeak-config-file config)))
      (list (shepherd-service
              (provision '(teamspeak))
              (requirement '(networking user-processes))
              (documentation "The TeamSpeak proprietary voice chat server daemon service.")
              (start #~(make-forkexec-constructor
                         (list #$(file-append package "/bin/ts3server")
                               (string-append "inifile=" #$teamspeak.ini))
                         #:user "teamspeak"
                         #:group "teamspeak"
                         #:pid-file "/var/run/teamspeak/teamspeak.pid"
                         #:directory "/var/lib/teamspeak"))
              (stop #~(make-kill-destructor))
              (actions (list (shepherd-configuration-action teamspeak.ini))))))))

(define teamspeak-service-type
  (service-type
    (name 'teamspeak)
    (extensions (list (service-extension account-service-type
                                         (const %teamspeak-accounts))
                      (service-extension activation-service-type
                                         teamspeak-activation)
                      (service-extension shepherd-root-service-type
                                         teamspeak-shepherd-service)))
    (description "Run the TeamSpeak proprietary voice chat server.")
    (default-value (teamspeak-configuration))))
