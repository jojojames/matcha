;;; matcha-omnisharp.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
;; (require 'omnisharp)

(defhydra matcha-omnisharp-project (:color blue)
  "Project"
  ("a" omnisharp-add-to-solution-current-file "Add To Solution")
  ("A" omnisharp-add-to-solution-dired-selected-files "Add to Solution Dired")
  ("r" omnisharp-remove-from-project-current-file "Remove From Solution")
  ("R" omnisharp-remove-from-project-dired-selected-files "Remove From Solution Dired")
  ("l" omnisharp-add-reference "Add Reference"))

(defhydra matcha-omnisharp-refactor (:color blue)
  "Refactor"
  ("m" omnisharp-rename "Rename")
  ("i" omnisharp-rename-interactively "Rename Interactively")
  ("r" omnisharp-run-code-action-refactoring "Code Action Refactoring"))

(defhydra matcha-omnisharp-navigation (:color blue :columns 4)
  "Navigation"
  ("g" omnisharp-go-to-definition "Go To Definition")
  ("G" omnisharp-go-to-definition-other-window "Go To Definition Other")
  ("u" omnisharp-find-usages "Find Usages")
  ("s" omnisharp-helm-find-symbols "Helm Find Symbols")
  ("i" omnisharp-find-implementations "Find Implementations")
  ("r" omnisharp-navigate-to-region "Region")
  ("m" omnisharp-navigate-to-solution-member "Solution Member")
  ("M" omnisharp-navigate-to-solution-member-other-window "Solution Member Other Window")
  ("f" omnisharp-navigate-to-solution-file "Solution File")
  ("F" omnisharp-navigate-to-solution-file-then-file-member "Solution Then File Member")
  ("c" omnisharp-navigate-to-current-file-member "Current File Member"))

(defhydra matcha-omnisharp-test (:color blue)
  "Test"
  ("a" omnisharp-unit-test-all "All")
  ("b" omnisharp-unit-test-fixture "Fixture")
  ("t" omnisharp-unit-test-single "Single"))

(defhydra matcha-omnisharp-help (:color blue)
  "Help"
  ("t" omnisharp-current-type-information "Current Type Information")
  ("T" omnisharp-current-type-information-to-kill-ring "Current Type To Kill Ring")
  ("s" omnisharp-start-omnisharp-server "Start Server")
  ("S" omnisharp-stop-server "Stop Server")
  ("r" omnisharp-reload-solution "Reload Solution"))

(defhydra matcha-omnisharp-mode (:color blue :columns 4)
  "CSharp"
  ("m" omnisharp-build-in-emacs "Build")
  ("p" matcha-omnisharp-project/body "Manage")
  ("r" matcha-omnisharp-refactor/body "Refactor")
  ("g" matcha-omnisharp-navigation/body "Navigate")
  ("t" matcha-omnisharp-test/body "Test")
  ("h" matcha-omnisharp-help/body "Help")
  ("u" omnisharp-auto-complete-overrides "Autocomplete Overrides")
  ("i" omnisharp-fix-usings "Fix Usings")
  ("=" omnisharp-code-format-entire-file "Code Format"))

(defun matcha-omnisharp-indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (if (region-active-p)
      (omnisharp-code-format-region)
    (omnisharp-code-format-entire-file)))

(defun matcha-omnisharp-set-launcher ()
  "Set `hydra' launcher for `omnisharp'."
  (matcha-set-format-command
   :mode 'csharp-mode :command #'matcha-omnisharp-indent-region-or-buffer)
  (matcha-set-test-command
   :mode 'csharp-mode :command #'matcha-omnisharp-test/body)
  (matcha-set-mode-command
   :mode 'csharp-mode :command #'matcha-omnisharp-mode/body))

(provide 'matcha-omnisharp)
;;; matcha-omnisharp.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
