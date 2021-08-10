;;; notmuch-update.el --- Update and reindex messages with an interval  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Splinter Suidman

;; Author: Splinter Suidman
;; Package-Requires: (notmuch)

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Run a command to update the notmuch index with a regular interval.

;;; Code:

(require 'notmuch)

(defvar notmuch-update-interval 60
  "The interval in seconds between consecutive updates.")

(defvar notmuch-update-command (concat notmuch-command " new")
  "The update command. This command is run using sh.

Example: mbsync --all && notmuch new")

(defvar notmuch-update-pre-hook nil
  "Hook to run before running the `notmuch-update-command'.")

(defvar notmuch-update-post-hook nil
  "Hook to run after running the `notmuch-update-command'.")

(defvar notmuch-update--timer nil
  "The notmuch update timer.")

(defun notmuch-update--sentinel (proc event)
  "Function to run after the notmuch update process is run."
  (when (or (not (eq (process-status proc) 'exit))
          (/= (process-exit-status proc) 0))
    (message "notmuch: Update process returned with non-zero exit code"))
  (run-hooks 'notmuch-update-post-hook))

;;;###autoload
(defun notmuch-update ()
  "Run the `notmuch-update-command'."
  (interactive)
  (run-hooks 'notmuch-update-pre-hook)
  (make-process :name "notmuch-update"
                :command `("sh" "-c" ,notmuch-update-command)
                :sentinel 'notmuch-update--sentinel))

(defun notmuch-update--init-timer ()
  "Initialise `notmuch-update--timer' if it is not nil."
  (unless notmuch-update--timer
    (setq notmuch-update--timer
          (run-at-time 0 notmuch-update-interval 'notmuch-update))))

(defun notmuch-update--cancel-timer ()
  "Cancel the `notmuch-update--timer'."
  (when notmuch-update--timer
    (cancel-timer notmuch-update--timer)
    (setq notmuch-update--timer nil)))

;;;###autoload
(defun notmuch-update-add-hook ()
  "Add hook to start timer when `notmuch-hello-mode' is entered."
  (add-hook 'notmuch-hello-mode-hook 'notmuch-update--init-timer))

;;;###autoload
(defun notmuch-update-start ()
  "Start the update timer."
  (notmuch-update--init-timer))

(provide 'notmuch-update)

;;; notmuch-update.el ends here
