;;; matcha-agent-shell.el --- Integration with Transient. -*- lexical-binding: t -*-

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
(require 'agent-shell nil t)

(transient-define-prefix matcha-agent-shell-help-menu ()
  "Transient menu for `agent-shell' commands."
  [["Navigation"
    ("<tab>" "Next item" agent-shell-next-item)
    ("<backtab>" "Previous item" agent-shell-previous-item)
    ("z" "Toggle Fragment" agent-shell-ui-toggle-fragment)
    ("Z" "Toggle All" agent-shell-ui-toggle-all-fragments)]
   ["Insert"
    ("!" "Shell command" agent-shell-insert-shell-command-output)
    ("@" "Send File" agent-shell-insert-file)
    ("d" "Send Dwim" agent-shell-send-dwim)
    ("t" "Send to" agent-shell-send-region-to)]
   ["Set"
    ("m" "Cycle modes" agent-shell-cycle-session-mode)
    ("o" "Set mode" agent-shell-set-session-mode) ;; M -> o
    ("v" "Set model" agent-shell-set-session-model)]
   ["Session"
    ("u" "Resume Session" agent-shell-resume-session)
    ("C" "Interrupt" agent-shell-interrupt)
    ("f" "Fork" agent-shell-fork)
    ("R" "Restart (drop history)" agent-shell-restart)
    ("r" "Reload (keep history)" agent-shell-reload)]
   ["Shell"
    ("b" "Toggle" agent-shell-toggle)
    ("n" "New shell" agent-shell-new-shell) ;; N -> n
    ("?" "Canonical Menu" agent-shell-help-menu)]])

(defun matcha-agent-shell-set-launcher ()
  "Set up `agent-shell-mode' with `transient'."
  (matcha-set-mode-command
   :mode 'agent-shell-mode :command 'matcha-agent-shell-help-menu))

(provide 'matcha-agent-shell)
;;; matcha-agent-shell.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
