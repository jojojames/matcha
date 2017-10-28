;;; hydra-csharp.el --- Integration with Hydra. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/hydra-integrations
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: hydra, emacs
;; HomePage: https://github.com/jojojames/hydra-integrations

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
(require 'hydra-integration-base)
(require 'omnisharp)

(defhydra hydra-csharp-project (:color blue)
  "Project"
  ("a" omnisharp-add-to-solution-current-file "Add To Solution")
  ("A" omnisharp-add-to-solution-dired-selected-files "Add to Solution Dired")
  ("r" omnisharp-remove-from-project-current-file "Remove From Solution")
  ("R" omnisharp-remove-from-project-dired-selected-files "Remove From Solution Dired")
  ("l" omnisharp-add-reference "Add Reference"))

(defhydra hydra-csharp-refactor (:color blue)
  "Refactor"
  ("m" omnisharp-rename "Rename")
  ("i" omnisharp-rename-interactively "Rename Interactively")
  ("r" omnisharp-run-code-action-refactoring "Code Action Refactoring"))

(defhydra hydra-csharp-navigation (:color blue :columns 4)
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

(defhydra hydra-csharp-test (:color blue)
  "Test"
  ("a" omnisharp-unit-test-all "All")
  ("b" omnisharp-unit-test-fixture "Fixture")
  ("t" omnisharp-unit-test-single "Single"))

(defhydra hydra-csharp-help (:color blue)
  "Help"
  ("t" omnisharp-current-type-information "Current Type Information")
  ("T" omnisharp-current-type-information-to-kill-ring "Current Type To Kill Ring")
  ("s" omnisharp-start-omnisharp-server "Start Server")
  ("S" omnisharp-stop-server "Stop Server")
  ("r" omnisharp-reload-solution "Reload Solution"))

(defhydra hydra-csharp-mode (:color blue :columns 4)
  "CSharp"
  ("m" omnisharp-build-in-emacs "Build")
  ("p" hydra-csharp-project/body "Manage")
  ("r" hydra-csharp-refactor/body "Refactor")
  ("g" hydra-csharp-navigation/body "Navigate")
  ("t" hydra-csharp-test/body "Test")
  ("h" hydra-csharp-help/body "Help")
  ("u" omnisharp-auto-complete-overrides "Autocomplete Overrides")
  ("i" omnisharp-fix-usings "Fix Usings")
  ("=" omnisharp-code-format-entire-file "Code Format"))

(defun +csharp-indent-dwim ()
  "Indent csharp code."
  (interactive)
  (call-interactively #'+csharp-indent-region-or-buffer))

(defun +csharp-indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (if (region-active-p)
      (omnisharp-code-format-region)
    (omnisharp-code-format-entire-file)))

(+add-indent-command #'+csharp-indent-dwim '(csharp-mode))
(+add-test-command #'+hydra-csharp-test/body '(csharp-mode))
(+add-mode-command #'+hydra-csharp-mode/body '(csharp-mode))

(provide 'hydra-csharp)
;;; hydra-csharp.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
