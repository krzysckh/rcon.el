;;; rcon.el --- RCON client -*- lexical-binding: t -*-

;; Author: kpm <kpm@linux.pl>
;; Created: 13 Jan 2024
;; Keywords: network, rcon, server, game
;; URL: https://github.com/krzysckh/rcon.el
;;
;; Copyright (C) 2024 kpm <kpm@linux.pl>
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;     * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following disclaimer
;; in the documentation and/or other materials provided with the
;; distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;; This file is not part of GNU Emacs.

(defvar rcon-proc-name "rcon" "process name for rcon.el")
(defvar rcon-buffer-name "*rcon*" "buffer name for rcon.el")
(defvar rcon-response-handler #'message "function called every time server sent a message with the response as an argument")
(defvar rcon-id 0)

(defun i32->lst (n)
  (reverse
   (list
    (logand (ash n -24) #xff)
    (logand (ash n -16) #xff)
    (logand (ash n -8) #xff)
    (logand (ash n 0) #xff))))

(defun rcon-build-packet (cmd data)
  (let* ((rid (i32->lst rcon-id))
         (type (i32->lst cmd))
         (payload (string-to-list data))
         (len (i32->lst
               (+ (length rid)
                  (length type)
                  (length payload)
                  2))))
    (append len rid type payload '(0 0))))

(defun rcon-send-packet (packet)
  (with-current-buffer (get-buffer-create rcon-buffer-name)
    (let ((proc (get-process rcon-proc-name)))
      (while (not (process-live-p (get-process rcon-proc-name)))
        (sleep-for 0.1))
      (process-send-string proc (apply #'string packet)))))

(defun rcon-register (passwd)
  (rcon-send-packet (rcon-build-packet 3 passwd)))

(defun rcon-parse-packet (v)
  (let ((s (apply #'string (nthcdr 12 (string-to-list v)))))
    (substring s 0 (- (length s) 2))))
   
(defun rcon-buf-change-hook (start end length)
  (with-current-buffer rcon-buffer-name
    (let* ((packet-pre (buffer-substring-no-properties start end))
           (packet (rcon-parse-packet packet-pre)))
      (funcall rcon-response-handler packet))))

;;;###autoload
(defun rcon-connect (ip port passwd)
  "connects to `ip` on port `port` and authenticates with password `passwd`"
  (open-network-stream
   rcon-proc-name
   rcon-buffer-name
   ip
   port
   :type 'plain
   :filter #'rcon-filter)
  (with-current-buffer (get-buffer-create rcon-buffer-name)
    (add-hook 'after-change-functions #'rcon-buf-change-hook nil t))
  (rcon-register passwd))

;;;###autoload
(defun rcon-stop ()
  "ends the rcon process"
  (delete-process "*rcon*"))

;;;###autoload
(defun rcon-execute-command (command)
  "executes `command` on the server. `(rcon-connect)` must be called before to establish a connection"
  (rcon-send-packet (rcon-build-packet 2 command)))

(provide 'rcon)
