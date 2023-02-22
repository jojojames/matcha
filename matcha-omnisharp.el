;;; matcha-omnisharp.el --- Integration with Transient. -*- lexical-binding: t -*-

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
;; (require 'omnisharp)

(transient-define-prefix matcha-omnisharp-project ()
  "Project"
  [["Project"
    ("a" "Add To Solution" omnisharp-add-to-solution-current-file)
    ("A" "Add to Solution Dired" omnisharp-add-to-solution-dired-selected-files)
    ("r" "Remove From Solution" omnisharp-remove-from-project-current-file)
    ("R" "Remove From Solution Dired" omnisharp-remove-from-project-dired-selected-files)
    ("l" "Add Reference" omnisharp-add-reference)]])

(transient-define-prefix matcha-omnisharp-refactor ()
  "Refactor"
  [["Refactor"
    ("m" "Rename" omnisharp-rename)
    ("i" "Rename Interactively" omnisharp-rename-interactively)
    ("r" "Code Action Refactoring" omnisharp-run-code-action-refactoring)]])

(transient-define-prefix matcha-omnisharp-navigation ()
  "Navigation"
  [["Navigation"
    ("g" "Go To Definition" omnisharp-go-to-definition)
    ("G" "Go To Definition Other" omnisharp-go-to-definition-other-window)
    ("u" "Find Usages" omnisharp-find-usages)
    ("s" "Helm Find Symbols" omnisharp-helm-find-symbols)
    ("i" "Find Implementations" omnisharp-find-implementations)
    ("r" "Region" omnisharp-navigate-to-region)
    ("m" "Solution Member" omnisharp-navigate-to-solution-member)
    ("M" "Solution Member Other Window" omnisharp-navigate-to-solution-member-other-window)
    ("f" "Solution File" omnisharp-navigate-to-solution-file)
    ("F" "Solution Then File Member" omnisharp-navigate-to-solution-file-then-file-member)
    ("c" "Current File Member" omnisharp-navigate-to-current-file-member)]])

(transient-define-prefix matcha-omnisharp-test ()
  "Test"
  [["Test"
    ("a" "All" omnisharp-unit-test-all)
    ("b" "Fixture" omnisharp-unit-test-fixture)
    ("t" "Single" omnisharp-unit-test-single)]])

(transient-define-prefix matcha-omnisharp-help ()
  "Help"
  [["Help"
    ("t" "Current Type Information" omnisharp-current-type-information)
    ("T" "Current Type To Kill Ring" omnisharp-current-type-information-to-kill-ring)
    ("s" "Start Server" omnisharp-start-omnisharp-server)
    ("r" "Reload Solution" omnisharp-reload-solution)
    ("S" "Stop Server" omnisharp-stop-server)]])

(transient-define-prefix matcha-omnisharp-mode ()
  "CSharp"
  [["Actions"
    ("m" "Build" omnisharp-build-in-emacs)
    ("r" "Refactor..." matcha-omnisharp-refactor)
    ("p" "Manage..." matcha-omnisharp-project)
    ("g" "Navigate..." matcha-omnisharp-navigation)
    ("t" "Test..." matcha-omnisharp-test)
    ("h" "Help..." matcha-omnisharp-help)
    ("u" "Autocomplete Overrides" omnisharp-auto-complete-overrides)
    ("i" "Fix Usings" omnisharp-fix-usings)
    ("=" "Code Format" omnisharp-code-format-entire-file)]])

(defun matcha-omnisharp-indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (if (region-active-p)
      (omnisharp-code-format-region)
    (omnisharp-code-format-entire-file)))

(defun matcha-omnisharp-set-launcher ()
  "Set `transient' launcher for `omnisharp'."
  (matcha-set-format-command
   :mode 'csharp-mode :command #'matcha-omnisharp-indent-region-or-buffer)
  (matcha-set-test-command
   :mode 'csharp-mode :command #'matcha-omnisharp-test)
  (matcha-set-mode-command
   :mode 'csharp-mode :command #'matcha-omnisharp-mode))

(provide 'matcha-omnisharp)
;;; matcha-omnisharp.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
