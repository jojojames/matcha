;;; matcha-dape.el --- Integration with Transient. -*- lexical-binding: t -*-

;; Copyright (C) 2024 James Nguyen

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
(require 'dape nil t)

(transient-define-prefix matcha-dape ()
  "Dape"
  [["Debug"
    ("d" "M-x dape" dape)
    ("p" "Pause" dape-pause)
    ("r" "Restart" dape-restart)
    ("R" "REPL" dape-repl)
    ("D" "Disconnect-Quit" dape-disconnect-quit)
    ("q" "Quit" dape-quit)]
   ["Step"
    ("c" "Continue" dape-continue)
    ("n" "Next" dape-next)
    ("s" "Step In" dape-step-in)
    ("o" "Step Out" dape-step-out)]
   ["Info"
    ("i" "Info" dape-info)
    ("m" "Read Memory" dape-read-memory)
    ("x" "Evaluate Expression" dape-evaluate-expression)
    ("w" "Watch DWIM" dape-watch-dwim)]
   ["Breakpoint"
    ("b" "Toggle Breakpoint" dape-breakpoint-toggle)
    ("B" "Remove All" dape-breakpoint-remove-all)
    ("l" "Breakpoint Log" dape-breakpoint-log)
    ("e" "Breakpoint Expression" dape-breakpoint-expression)
    ("h" "Breakpoint Hits" dape-breakpoint-hits)]
   ["Stacks"
    ("t" "Select Thread" dape-select-thread)
    ("S" "Select Stack" dape-select-stack)
    (">" "Select Stack Down" dape-stack-select-down)
    ("<" "Select Stack Up" dape-stack-select-up)]])

(defun matcha-dape-set-launcher ()
  "Set `transient' launcher for `dape'."
  (matcha-set-debug-command
   :mode 'dape-mode :command #'matcha-dape :minor-p t)
  (matcha-set-debug-command
   :mode matcha-dape-modes
   :command #'matcha-dape))

(provide 'matcha-dape)
;;; matcha-dape.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
