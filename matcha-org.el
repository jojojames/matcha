;;; matcha-org.el --- Integration with Transient. -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/matcha
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: transient, emacs
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
;;; Integration with Transient.

;;; Code:
(require 'matcha-base)
(require 'org)

(transient-define-prefix matcha-org-babel ()
  "Babel"
  ["Babel"
   ("e" "Execute Source Block" org-babel-execute-src-block )
   ("'" "Edit Source" org-edit-src-code )])

(transient-define-prefix matcha-org-hyperlink ()
  "Org Links"
  [["Navigate"
    ("g" "Follow Link" org-open-at-point)
    ("n" "Next Link" org-next-link)
    ("p" "Previous Link" org-previous-link)
    ("r" "Occur Links" org-occur-link-in-agenda-files)]
   ["Manage"
    ("l" "Store Link" org-store-link)
    ("i" "Insert Link" org-insert-link)
    ("t" "Toggle Link Display" org-toggle-link-display)]]
  [:hide (lambda () t)
         ("s" org-store-link)])

(transient-define-prefix matcha-org-time ()
  "Time"
  [["Insert"
    ("t" "Timestamp" org-time-stamp)
    ("T" "Inactive Timestamp" org-time-stamp-inactive)
    ("s" "Schedule Item" org-schedule)
    ("d" "Deadline" org-deadline)
    ("c" "Goto Calendar" org-goto-calendar)
    ("C" "Date from Calendar" org-date-from-calendar)]
   ["Timer"
    ("0" "Start Timer" org-timer-start)
    ("9" "Stop Timer" org-timer-stop)
    ("8" "Pause/Continue Timer" org-timer-pause-or-continue)
    ("7" "Insert Timer String" org-timer)
    ("6" "Insert Timer Item" org-timer-item)]
   ["Manage"
    ("D" "Change Date..." matcha-org-change-date)
    ("y" "Evaluate Time Range" org-evaluate-time-range)
    ("Z" "Custom Time Format" org-toggle-time-stamp-overlays)]])

(transient-define-prefix matcha-org-change-date ()
  "Change Date"
  ["Change Date"
   ("l" "1 Day Later" org-shiftright)
   ("h" "1 Day Before" org-shiftleft)
   ("k" "1 ... Later" org-shiftup)
   ("j" "1 ... Before" org-shiftdown)])

(transient-define-prefix matcha-org-editing ()
  "Edit"
  [
   :description
   (lambda () (propertize
          (format "Org: %s" (matcha-heading-current-file))
          'face 'org-level-1))
   ["Insert"
    ("m" "Heading" org-meta-return)
    ("M" "Heading Under" org-insert-heading-respect-content)
    ("t" "Todo" org-insert-todo-heading)
    ("T" "Todo Under" org-insert-todo-heading-respect-content)]
   ["Promotion"
    ("<left>" "Promoto Heading" org-do-promote)
    ("<right>" "Demote Heading" org-do-demote)
    ("S-<left>" "Promote Subtree" org-promote-subtree)
    ("S-<right>" "Demote Subtree" org-demote-subtree)
    ("<up>" "Move Subtree Up" org-move-subtree-up)
    ("<down>" "Move Subtree Down" org-move-subtree-down)]
   ["Mark"
    ("e" "Element" org-mark-element :transient t)
    ("@" "Subtree" org-mark-subtree :transient t)]]
  [["Subtree"
    ("x" "Cut" org-cut-subtree)
    ("w" "Copy" org-copy-subtree)
    ("y" "Paste" org-paste-subtree)
    ("Y" "Yank" org-yank)
    ("W" "Clone" org-clone-subtree-with-time-shift)]
   ["Modify"
    ("r" "Refile" org-refile)
    ("^" "Sort" org-sort)
    ("*" "Toggle Heading" org-toggle-heading)]
   ["Narrow"
    ("ns" "Narrow to Subtree" org-narrow-to-subtree)
    ("nb" "Narrow to Block" org-narrow-to-block)
    ("nw" "Widen" widen)]])

(transient-define-prefix matcha-org-mode ()
  "Org Mode"
  [
   :description
   (lambda () (propertize
          (format "Org: %s" (matcha-heading-current-file))
          'face 'org-level-1))
   ["Motion"
    ("n" "Next Heading" org-next-visible-heading :transient t)
    ("p" "Previous Heading" org-previous-visible-heading :transient t)
    ("f" "Forward Level" org-forward-heading-same-level :transient t)
    ("b" "Backward Level" org-backward-heading-same-level :transient t)
    ("u" "Up Heading" outline-up-heading :transient t)
    ("j" "Goto" org-goto)]
   ["Misc"
    ("e" "Editing..." matcha-org-editing)
    ("B" "Babel..." matcha-org-babel)
    ("t" "Time..." matcha-org-time)
    ("l" "Links..." matcha-org-hyperlink)
    ("r" "Reveal" org-reveal)]])

(defun matcha-org-set-launcher ()
  "Set `transient' launcher for `org'."
  (matcha-set-mode-command :mode 'org-mode :command #'matcha-org-mode)
  (matcha-set-eval-command :mode 'org-mode :command #'matcha-org-editing))

(provide 'matcha-org)
;;; matcha-org.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
