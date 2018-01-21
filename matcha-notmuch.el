;;; matcha-notmuch.el --- Integration with Hydra. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/matcha
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: hydra, emacs
;; HomePage: https://github.com/jojojames/matcha

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Integration with Hydra.

;;; Code:
(require 'matcha-base)
(require 'notmuch nil t)

(defhydra matcha-notmuch-tree-mode (:color blue :columns 4)
  "Tree"
  ("?" (notmuch-tree-close-message-pane-and #'notmuch-help) "Help")
  ("q" notmuch-tree-quit "Quit")
  ("s" notmuch-tree-to-search "Search")
  ("m" (notmuch-tree-close-message-pane-and #'notmuch-mua-new-mail)
   "Compose")
  ("J" (notmuch-tree-close-message-pane-and #'notmuch-jump-search) "Jump")
  ("S" notmuch-search-from-tree-current-query "Search from Query")
  ("|" notmuch-show-pipe-message "Show Pipe Message")
  ("w" notmuch-show-save-attachments "Save Attachments")
  ("v" notmuch-show-view-all-mime-parts "View All Mime Parts")
  ("c" matcha-notmuch-show-stash-command/body "Show Stash Commands")
  ("b" notmuch-show-resend-message "Resend Message")
  ("$" (notmuch-tree-to-message-pane #'notmuch-show-toggle-process-crypto)
   "Process Crypto")
  ("f" (notmuch-tree-close-message-pane-and #'notmuch-show-forward-message)
   "Forward Message")
  ("r" (notmuch-tree-close-message-pane-and #'notmuch-show-reply-sender)
   "Reply To Sender")
  ("R" (notmuch-tree-close-message-pane-and #'notmuch-show-reply) "Reply")
  ("V" (notmuch-tree-close-message-pane-and #'notmuch-show-view-raw-message)
   "View Raw Message")
  ("RET" notmuch-tree-show-message "Show Message")
  ("x" notmuch-tree-quit "Quit")
  ("A" notmuch-tree-archive-thread "Archive Thread")
  ("a" notmuch-tree-archive-message-then-next "Archive Message Then Next")
  ("z" notmuch-tree-to-tree "Tree to Tree")
  ("n" notmuch-tree-next-matching-message "Next Matching Message")
  ("p" notmuch-tree-prev-matching-message "Previous Matching Message")
  ("N" notmuch-tree-next-message "Next Message")
  ("P" notmuch-tree-prev-message "Previous Message")
  ("C-p" notmuch-tree-prev-thread "Previous Thread")
  ("C-n" notmuch-tree-next-thread "Next Thread")
  ("k" notmuch-tag-jump "Tag Jump")
  ("-" notmuch-tree-remove-tag "Remove Tag")
  ("+" notmuch-tree-add-tag "Add Tag")
  ("*" notmuch-tree-tag-thread "Tag Thread")
  ("e" notmuch-tree-resume-message "Resume Message"))

(defhydra matcha-notmuch-search-mode (:color blue :columns 4)
  "Search"
  ("J" notmuch-jump-search "Jump Search")
  ("K" notmuch-tag-jump "Tag Jump")
  ("L" notmuch-search-filter "Search Filter")
  ("C" compose-mail-other-frame "Compose Mail Other Frame")
  ("O" notmuch-search-toggle-order "Toggle Order")
  ("R" notmuch-search-reply-to-thread "Reply to Thread")
  ("Z" notmuch-tree-from-search-current-query "Tree from current Query")
  ("a" notmuch-search-archive-thread "Archive Thread")
  ("c" compose-mail "Compose Mail")
  ("d" +notmuch-search-toggle-delete "Toggle Delete")
  ("m" matcha-notmuch-common/body "Common Commands")
  ("n" notmuch-search-next-thread "Next Thread")
  ("o" compose-mail-other-window "Compose Mail Other Window")
  ("p" notmuch-search-previous-thread "Previous Thread")
  ("q" notmuch-bury-or-kill-this-buffer "Bury or Kill Buffer")
  ("r" notmuch-search-reply-to-thread-sender "Reply to Thread Sender")
  ("t" notmuch-search-filter-by-tag "Filter by Tag")
  ("z" matcha-notmuch-search-stash/body "Search Stash Map")
  ("*" notmuch-search-tag-all "Tag All")
  ("-" notmuch-search-remove-tag "Remove Tag")
  ("+" notmuch-search-add-tag "Add Tag")
  ("<" notmuch-search-first-thread "First Thread")
  (">" notmuch-search-last-thread "Last Thread")
  ("RET" notmuch-search-show-thread "Show Thread"))

(defhydra matcha-notmuch-search-stash (:color :columns 4)
  "Stash"
  ("i" notmuch-search-stash-thread-id "Stash Thread Id")
  ("q" notmuch-stash-query "Stash Query")
  ("?" notmuch-subkeymap-help "Help"))

(defhydra matcha-notmuch-common (:color blue :columns 4)
  "Common Commands"
  ("?" notmuch-help "Help")
  ("q" notmuch-bury-or-kill-this-buffer "Bury or Kill Buffer")
  ("s" notmuch-search "Search")
  ("z" notmuch-tree "Tree")
  ("m" notmuch-mua-new-mail "Mua New Mail")
  ("gr" notmuch-refresh-this-buffer "Refresh this Buffer")
  ("gR" notmuch-refresh-all-buffers "Refresh all Buffers")
  ("Z" notmuch-poll-and-refresh-this-buffer "Poll and Refresh this Buffer")
  ("J" notmuch-jump-search "Jump Search"))

(defhydra matcha-notmuch-hello-mode (:color blue :columns 4)
  "Hello"
  ("m" matcha-notmuch-common/body "Common Commands"))

(defhydra matcha-notmuch-show-stash-commands (:color blue :columns 4)
  "Show Stash Commands"
  ("c" notmuch-show-stash-cc "CC")
  ("d" notmuch-show-stash-date "Date")
  ("F" notmuch-show-stash-filename "Filename")
  ("f" notmuch-show-stash-from "From")
  ("i" notmuch-show-stash-message-id "Message Id")
  ("I" notmuch-show-stash-message-id-stripped "Message Id Stripped")
  ("s" notmuch-show-stash-subject "Subject")
  ("T" notmuch-show-stash-tags "Tags")
  ("t" notmuch-show-stash-to "To")
  ("l" notmuch-show-stash-mlarchive-link "Archive Link")
  ("L" notmuch-show-stash-mlarchive-link-and-go "Archive Link and Go")
  ("G" notmuch-show-stash-git-send-email "Git Send Email")
  ("?" notmuch-subkeymap-help "Help"))

(defhydra matcha-notmuch-show-part (:color blue :columns 4)
  "Show Part"
  ("s" notmuch-show-save-part "Save Part")
  ("v" notmuch-show-view-part "View Part")
  ("o" notmuch-show-interactively-view-part "Show Interactively View Part")
  ("|" notmuch-show-pipe-part "Show Pipe Part")
  ("m" notmuch-show-choose-mime-of-part "Show Choose Mime of Part")
  ("?" notmuch-subkeymap-help "Help"))

(defhydra matcha-notmuch-show-mode (:color blue :hint nil)
  "

   Show: %(notmuch-show-get-subject)

    ^^Compose^^             ^^Tags^^         ^^Archive Then^^       ^^Misc^^
  ------------------------------------------------------------------------------
  _F_ Forward Open      ^_d_ Delete    ^^_A_ -> Message     ^_l_ Filter Thread
  _R_ Reply             ^_K_ Jump      ^^_a_ Msg Or Thread  ^_m_ Common
  _r_ Reply Sender      ^_-_ Remove    ^^_X_ -> Exit        ^_w_ Save Attachments
  _S_ Resend Message    ^_+_ Add       ^^_x_ Msg Or Exit    ^_._ Attachments
  _e_ Resume Draft      ^_*_ Tag All                      ^^^^^_|_ Pipe
  _f_ Forward Message

    ^^Navigate^^                ^^View^^                ^^Toggle^^
  ------------------------------------------------------------------------------
  _j_ Next Thread         ^^_N_ Next Message      ^^_h_ Headers
  _k_ Previous Thread     ^^_P_ Previous Message  ^^_t_ Truncate Lines
  _n_ Next Message        ^^_V_ Raw Message       ^^_!_ Elide Non Matching
  _p_ Previous Message    ^^_#_ Print Message     ^^_$_ Process Crypto
  _y_ Rewind              ^^_Z_ Tree              ^^_<_ Thread Indentation
  _z_ Advance             ^^_c_ Stash             ^^_T_ All Messages
                                                ^^^^^^^^_RET_ Message

"
  ("l" notmuch-show-filter-thread)
  ("m" matcha-notmuch-common/body)
  ("w" notmuch-show-save-attachments)
  ("." matcha-notmuch-show-part/body)
  ("|" notmuch-show-pipe-message)
  ("F" notmuch-show-forward-open-messages)
  ("R" notmuch-show-reply)
  ("r" notmuch-show-reply-sender)
  ("S" notmuch-show-resend-message)
  ("e" notmuch-show-resume-message)
  ("f" notmuch-show-forward-message)
  ("h" notmuch-show-toggle-visibility-headers)
  ("t" toggle-truncate-lines)
  ("!" notmuch-show-toggle-elide-non-matching)
  ("$" notmuch-show-toggle-process-crypto)
  ("<" notmuch-show-toggle-thread-indentation)
  ("T" notmuch-show-open-or-close-all)
  ("RET" notmuch-show-toggle-message)
  ("d" +notmuch-show-toggle-delete)
  ("K" notmuch-tag-jump)
  ("-" notmuch-show-remove-tag)
  ("+" notmuch-show-add-tag)
  ("*" notmuch-show-tag-all)
  ("A" notmuch-show-archive-thread-then-next)
  ("X" notmuch-show-archive-thread-then-exit)
  ("a" notmuch-show-archive-message-then-next-or-next-thread)
  ("x" notmuch-show-archive-message-then-next-or-exit)
  ("j" notmuch-show-next-thread-show :color red)
  ("k" notmuch-show-previous-thread-show :color red)
  ("n" notmuch-show-next-open-message :color red)
  ("p" notmuch-show-previous-open-message :color red)
  ("y" notmuch-show-rewind)
  ("z" notmuch-show-advance-and-archive)
  ("N" notmuch-show-next-message :color red)
  ("P" notmuch-show-previous-message :color red)
  ("V" notmuch-show-view-raw-message)
  ("#" notmuch-show-print-message)
  ("Z" notmuch-tree-from-show-current-query)
  ("c" matcha-notmuch-show-stash-command/body))

(defun matcha-notmuch-set-launcher ()
  "Set `hydra' launcher for `notmuch'."
  (matcha-set-mode-command
   :mode 'notmuch-hello-mode :command #'matcha-notmuch-hello-mode/body)
  (matcha-set-mode-command
   :mode 'notmuch-show-mode :command #'matcha-notmuch-show-mode/body)
  (matcha-set-mode-command
   :mode 'notmuch-tree-mode :command #'matcha-notmuch-tree-mode/body)
  (matcha-set-mode-command
   :mode 'notmuch-search-mode :command #'matcha-notmuch-search-mode/body))

(provide 'matcha-notmuch)
;;; matcha-notmuch.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
