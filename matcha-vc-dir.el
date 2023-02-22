;;; matcha-vc-dir.el --- Integration with Transient. -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/matcha
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: transient, emacs, vc-dir
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
(require 'vc-dir nil t)

(transient-define-prefix matcha-vc-dir-log ()
  "Log"
  [["Log"
    ("l" "Log" vc-print-log)
    ("r" "Root Log" vc-print-root-log)
    ("i" "Log Incoming" vc-log-incoming)
    ("o" "Log Outgoing" vc-log-outgoing)]])

(transient-define-prefix matcha-vc-dir-branch ()
  "Branch"
  [["Branch"
    ("c" "Create" vc-create-tag)
    ("l" "Print Log" vc-print-branch-log)
    ("s" "Retrieve" vc-retrieve-tag)]])

(transient-define-prefix matcha-vc-dir ()
  "VC"
  [["Actions"
    ("c" "Next Action" vc-next-action)
    ("r" "Register File" vc-register)
    ("F" "Sync/Pull from Repository" vc-update)
    ("p" "Push Changes" vc-push)
    ("x" "Revert File" vc-revert)]
   ["Diff"
    ("b" "Annotate/Blame" vc-annotate)
    ("d" "Diff" vc-diff)
    ("D" "Diff against Root" vc-root-diff)]
   ["Misc"
    ("b" "Branching..." matcha-vc-dir-branch)
    ("l" "Logging..." matcha-vc-dir-log)]])

(defun matcha-vc-dir-set-launcher ()
  "Set up `dired' with `transient'."
  (matcha-set-mode-command :mode 'vc-dir-mode :command #'matcha-vc-dir))

(provide 'matcha-vc-dir)
;;; matcha-vc-dir.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
