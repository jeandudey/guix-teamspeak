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
                   "logpath=/var/log/teamspeak\n"
                   "logappend=1"))

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
