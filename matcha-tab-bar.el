;;; matcha-tab-bar.el --- Integration with Transient. -*- lexical-binding: t -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/matcha
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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
(require 'tab-bar nil)

(transient-define-prefix matcha-tab-bar ()
  "Tab Bar"
  [["New"
    ("N" "Duplicate Tab" tab-duplicate)
    ("2" "New Tab To" tab-new-to)
    ("n" "New Tab" tab-new)]
   ["Close"
    ("0" "Close Tab" tab-close)
    ("1" "Close Other Tabs" tab-close-other)
    ("u" "Undo Close" tab-undo)]
   ["Navigate"
    ("j" "Next Tab" tab-next)
    ("k" "Previous Tab" tab-previous)
    ("RET" "Switch Tab" tab-switch)]]
  [["Move"
    ("m" "Move Tab" tab-move)
    ("M" "Move Tab To" tab-move-to)]
   ["Manage"
    ("G" "Group Tab" tab-group)
    ("r" "Rename Tab" tab-rename)
    ("D" "Detach Tab" tab-detach)]
   ["Open"
    ("b" "Buffer in Other Tab" switch-to-buffer-other-tab)
    ("f" "Find File in Other Tab" find-file-other-tab)
    ("f" "Find File in Other Tab" find-file-other-tab)
    ("R" "Find File Read-Only in Other Tab" find-file-read-only-other-tab)
    ("t" "Other Tab Prefix" other-tab-prefix)]])

(provide 'matcha-tab-bar)
;;; matcha-tab-bar.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
