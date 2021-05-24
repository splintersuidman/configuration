;;; notmuch-notify.el --- Send notifications for new messages with notmuch  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Splinter Suidman

;; Author: Splinter Suidman

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

;; The `notmuch-notify' function will search using notmuch all
;; messages tagged with the tags 'notification' and 'unread'. For
;; every message, it will send a notification using notify-send
;; consisting of the author(s) and the subject of the message.
;; Finally, it removes the 'notification' tag from those messages.

;;; Code:

(defun notmuch-notify-command-default (msg)
  `("notify-send" ,(plist-get msg :authors) ,(plist-get msg :subject)))

(defvar notmuch-notify-command 'notmuch-notify-command-default
  "Command used to send a notification.

This is a function with a message as a parameter, that must
return a list of which the first element is the program to run,
and the other elements are the arguments which are passed to the
program.

The message is a plist with the following attributes: `:thread',
`:timestamp', `:date_relative', `:matched', `:total', `:authors',
`:subject', `:query', `:tags'.")

(defvar notmuch-notify--output ""
  "Variable used to write the output of notmuch search to.")

(defvar notmuch-notify--process nil
  "The notify process, used to check whether a process is already running.")

(defun notmuch-notify--filter (proc string)
  "Filter to pass to `make-process' which writes the process'
output to the string `notmuch-notify--output'."
  (setq notmuch-notify--output (concat notmuch-notify--output string)))

(defun notmuch-notify--sentinel (proc event)
  "Sentinel to pass to `make-process' which parses the messages
from `notmuch-notify--output' (in the sexp format of notmuch
search) and sends for each of these messages a notification. Then
it removes the notification tag from all messages with that tag."
  (setq notmuch-notify--process nil)
  (dolist (msg (read notmuch-notify--output))
    (make-process :name "notmuch-notify"
                  :command (funcall notmuch-notify-command msg)))
  (make-process :name "notmuch-notify-clear-tag"
                :command `(,notmuch-command "tag" "-notification" "--" "tag:notification")))

;;;###autoload
(defun notmuch-notify ()
  "Search using notmuch all messages tagged with the tags
'notification' and 'unread'. For every message, send a
notification using notify-send consisting of the author(s) and
the subject of the message. Finally, remove the 'notification'
tag from those messages."
  (interactive)
  (setq notmuch-notify--output "")
  (unless (and notmuch-notify--process (process-live-p notmuch-notify--process))
    (setq notmuch-notify--process
          (make-process :name "notmuch-notify"
                        :command `(,notmuch-command "search" "--format=sexp" "tag:notification and tag:unread")
                        :filter 'notmuch-notify--filter
                        :sentinel 'notmuch-notify--sentinel))))

(provide 'notmuch-notify)

;;; notmuch-notify.el ends here
