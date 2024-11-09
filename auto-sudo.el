;;; auto-sudo.el --- Automatically reopen current file with sudo -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/auto-sudo
;; Version: 0.1.0
;; Keywords: files
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Automatically reopen current file with sudo

;;; Code:

(defcustom auto-sudo-edit-ask nil
  "Whether to ask for confirmation for auto sudo edit.
See `auto-sudo-edit-mode'."
  :group 'files
  :type 'boolean)

(declare-function tramp-sh-handle-file-writable-p "tramp")
(declare-function tramp-tramp-file-p "tramp")
(declare-function make-tramp-file-name "tramp")
(declare-function tramp-make-tramp-file-name "tramp")
(declare-function tramp-file-name-hop "tramp")
(declare-function tramp-file-name-localname "tramp")
(declare-function tramp-file-name-port "tramp")
(declare-function tramp-file-name-host "tramp")
(declare-function tramp-file-name-user "tramp")
(declare-function tramp-file-name-method "tramp")
(declare-function tramp-dissect-file-name "tramp")
(declare-function tramp-get-remote-uid "tramp")

(defun auto-sudo-edit-current-user (path)
  "Return the current user for the given PATH.

If PATH is a remote file, extract the remote UID using `tramp-dissect-file-name'
 and `tramp-get-remote-uid'.

Otherwise, return the local user login name."
  (if (tramp-tramp-file-p path)
      (tramp-get-remote-uid (tramp-dissect-file-name path) 'string)
    (user-login-name)))

(defun auto-sudo-edit-path-from-tramp-ssh (curr-file new-user)
  "Join Tramp SSH-like path components with a new username.
Argument CURR-FILE is a tramp path, NEW-USER is the user for sudo."
  (require 'tramp)
  (let* ((file-name (tramp-dissect-file-name curr-file))
         (method (tramp-file-name-method file-name))
         (user (tramp-file-name-user file-name))
         (host (tramp-file-name-host file-name))
         (port (tramp-file-name-port file-name))
         (localname (tramp-file-name-localname file-name))
         (hop (tramp-file-name-hop file-name))
         (new-method "sudo")
         (new-host host)
         (new-port port)
         (new-localname localname)
         (new-hop (format "%s%s:%s%s%s|" (or hop "")
                          method
                          (if user
                              (concat user "@")
                            "")
                          host
                          (if port
                              (concat port "#")
                            ""))))
    (if (equal method "sudo")
        curr-file
      (tramp-make-tramp-file-name
       (make-tramp-file-name
        :method new-method
        :user new-user
        :host new-host
        :port new-port
        :localname new-localname
        :hop new-hop)))))

(defun auto-sudo-edit-file-owner (path)
  "Determine the login name of the user PATH belongs to."
  (file-attribute-user-id (file-attributes path 'string)))

(defun auto-sudo-edit-get-remote-info (file)
  "Return cons (USER . TRAMP-PATH) for FILE.
If file cannot be opened with sudo, USER is nil."
  (let* ((file-owner
          (auto-sudo-edit-file-owner
           file))
         (tramp-path
          (if (tramp-tramp-file-p file)
              (auto-sudo-edit-path-from-tramp-ssh file
                                                       file-owner)
            (concat "/sudo::" file))))
    (if (and
         file-owner
         (not (string= file-owner (auto-sudo-edit-current-user
                                   file)))
         (not (equal file tramp-path)))
        (cons file-owner tramp-path)
      (cons nil file))))

(defun auto-sudo-edit-setup ()
  "Set up the for the automatic behavior of editing remote files with sudo.

This function adds or remove itself from `read-only-mode-hook' to check and
maybe reopen current buffer as sudo user.

If there is no file name, no remote info, or no valid remote path,
the hook is removed from the `read-only-mode-hook'.

If the buffer is read-only, the hook is added back to the
`read-only-mode-hook' to handle future changes.

If `auto-sudo-edit-ask' is non nil, also prompt the user for
confirmation before reopening."
  (require 'tramp)
  (let ((remote-info
         (and buffer-file-name
              (auto-sudo-edit-get-remote-info buffer-file-name))))
    (cond ((or (not buffer-file-name)
               (not remote-info)
               (not (car remote-info)))
           (remove-hook 'read-only-mode-hook
                        #'auto-sudo-edit-setup
                        t))
          ((and buffer-read-only)
           (add-hook 'read-only-mode-hook #'auto-sudo-edit-setup
                     nil
                     t))
          ((and
            buffer-file-name
            (not buffer-read-only)
            remote-info
            (car remote-info)
            (cdr remote-info))
           (when-let* ((tramp-path (and
                                   (not (and (tramp-tramp-file-p
                                              buffer-file-name)
                                             (tramp-sh-handle-file-writable-p
                                              buffer-file-name)))
                                   (or
                                    (not auto-sudo-edit-ask)
                                    (yes-or-no-p (format
                                                  "Reopen this file as %s?"
                                                  (car remote-info))))
                                   (cdr remote-info))))
             (set-visited-file-name tramp-path t)
             (revert-buffer t t))))))

;;;###autoload
(define-minor-mode auto-sudo-edit-mode
  "Allow automatic editing for remote files with sudo when this mode on.

After a buffer is loaded from a file that belongs to sudo user and buffer
is not read-only reopen it with tramp.

If buffer is read-only, reopen it after read-only mode is toggled.

If `auto-sudo-edit-ask' is non nil, also prompt the user for
confirmation before reopening."
  :global t
  :group 'files
  :lighter " asudo"
  (remove-hook 'find-file-hook #'auto-sudo-edit-setup)
  (when auto-sudo-edit-mode
    (add-hook 'find-file-hook #'auto-sudo-edit-setup)))

(provide 'auto-sudo)
;;; auto-sudo.el ends here