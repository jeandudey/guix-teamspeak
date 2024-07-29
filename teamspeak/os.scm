;;; SPDX-FileCopyrightText: © 2024 Jean-Pierre De Jesus DIAZ <me@jeandudey.tech>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (teamspeak os)
  #:use-module (gnu)
  #:use-module (gnu services networking)
  #:use-module (teamspeak service)
  #:export (teamspeak-operating-system))

(define teamspeak-operating-system
  (operating-system
    (host-name "tsexample")
    (timezone "Europe/Madrid")
    (locale "en_US.utf8")
    (bootloader (bootloader-configuration
                  (bootloader grub-bootloader)
                  (targets '("/dev/sdX"))))
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))
    (services
      (cons* (service teamspeak-service-type)
             %base-services))))

teamspeak-operating-system
