;;; matcha-proced.el --- Integration with Transient. -*- lexical-binding: t -*-

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
(require 'proced nil t)

(transient-define-prefix matcha-proced-transient ()
  "Proced command menu."

  ;; Row 1
  [["Movement"
    ("n" "Next line"        next-line)
    ("p" "Previous line"    previous-line)
    ("SPC" "Next line"      next-line)
    ("S-SPC" "Previous"     previous-line)]

   ["Marking"
    ("m" "Mark"             proced-mark)
    ("u" "Unmark"           proced-unmark)
    ("DEL" "Unmark back"    proced-unmark-backward)
    ("M" "Mark all"         proced-mark-all)
    ("U" "Unmark all"       proced-unmark-all)
    ("t" "Toggle marks"     proced-toggle-marks)
    ("C" "Mark children"    proced-mark-children)
    ("P" "Mark parents"     proced-mark-parents)]

   ["Filtering / View"
    ("f" "Filter"           proced-filter-interactive)
    ("RET" "Refine"         proced-refine)
    ("T" "Toggle tree"      proced-toggle-tree)
    ("o" "Omit processes"   proced-omit-processes)
    ("F" "Format"           proced-format-interactive)]]

  ;; Row 2
  [["Sorting (prefix s)"
    ("sc" "CPU %"           proced-sort-pcpu)
    ("sm" "Memory %"        proced-sort-pmem)
    ("sp" "PID"             proced-sort-pid)
    ("ss" "Start time"      proced-sort-start)
    ("sS" "Sort (interactive)" proced-sort-interactive)
    ("st" "Time"            proced-sort-time)
    ("su" "User"            proced-sort-user)]

   ["Operate"
    ("k" "Kill (signal)"    proced-send-signal)
    ("x" "Send signal"      proced-send-signal)
    ("r" "Renice"           proced-renice)]

   ["Misc"
    ("h" "Describe mode"    describe-mode)
    ("?" "Help"             proced-help)
    ("u" "Undo"             proced-undo)]])

(defun matcha-proced-set-launcher ()
  "Set up `proced-mode' with `transient'."
  (matcha-set-mode-command
   :mode 'proced-mode :command 'matcha-proced-transient))

(provide 'matcha-proced)
;;; matcha-proced.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
