;;; notmuch-notify.el --- Send notifications for new messages with notmuch  -*- lexical-binding: t; -*-

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

;; The `notmuch-notify' function will search using notmuch all
;; messages tagged with the tags 'notification' and 'unread'. For
;; every message, it will send a notification using notify-send
;; consisting of the author(s) and the subject of the message.
;; Finally, it removes the 'notification' tag from those messages.

;;; Code:

(require 'notmuch)

(defgroup notmuch-notify nil
  "Notifications for new mails in Notmuch."
  :prefix "notmuch-notify-"
  :group 'notmuch)

(defun notmuch-notify--command-default (msg)
  `("notify-send" ,(plist-get msg :authors) ,(plist-get msg :subject)))

(defcustom notmuch-notify-command 'notmuch-notify--command-default
  "Command used to send a notification.

This is a function with a message as a parameter, that must
return a list of which the first element is the program to run,
and the other elements are the arguments which are passed to the
program.

The message is a plist with the following attributes: `:thread',
`:timestamp', `:date_relative', `:matched', `:total', `:authors',
`:subject', `:query', `:tags'."
  :type 'function
  :group 'notmuch-notify)

;;;###autoload
(defun notmuch-notify ()
  "Search using notmuch all messages tagged with the tags
'notification' and 'unread'. For every message, send a
notification using notify-send consisting of the author(s) and
the subject of the message. Finally, remove the 'notification'
tag from those messages."
  (interactive)
  (let ((messages (notmuch-call-notmuch-sexp "search" "--format=sexp" "tag:notification and tag:unread")))
    (dolist (msg messages)
      (make-process :name "notmuch-notify"
                    :command (funcall notmuch-notify-command msg)))
    (notmuch-call-notmuch-process "tag" "-notification" "--" "tag:notification")))

(provide 'notmuch-notify)

;;; notmuch-notify.el ends here
