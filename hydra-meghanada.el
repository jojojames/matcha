;;; hydra-meghanada.el --- Integration with Hydra. -*- lexical-binding: t -*-

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

(defhydra hydra-meghanada-server (:color blue :columns 3)
  "Server"
  ("U" meghanada-update-server "Update")
  ("S" meghanada-server-start "Start")
  ("R" meghanada-restart "Restart")
  ("K" meghanada-server-kill "Kill")
  ("D" meghanada-client-direct-connect "Direct Connect")
  ("C" meghanada-client-connect "Connect")
  ("k" meghanada-client-disconnect "Disconnect")
  ("p" meghanada-client-ping "Ping Server")
  ("c" meghanada-clear-cache "Clear Cache"))

(defhydra hydra-meghanada-refactor (:color blue :columns 3)
  "Refactor"
  ("I" meghanada-import-all "Import All")
  ("o" meghanada-optimize-import "Optimize Imports")
  ("l" meghanada-local-variable "Make Local Variable")
  ("v" meghanada-local-variable "Make Local Variable")
  ("n" meghanada-local-variable "Make Local Variable")
  ("r" meghanada-local-variable "Make Local Variable")
  ("f" meghanada-code-beautify "Beautify Code")
  ("b" meghanada-code-beautify "Beautify Code"))

(defhydra hydra-meghanada-mode (:color blue :columns 3)
  "Meghanada"
  ("s" hydra-meghanada-server/body "Server")
  ("r" hydra-meghanada-refactor/body "Refactor")
  ("g" hydra-meghanada-navigate/body "Navigate")
  ("c" hydra-meghanada-compile/body "Compile")
  ("t" hydra-meghanada-test/body "Test")
  ("m" meghanada-run-task "Task")
  ("T" meghanada-run-task "Task"))

(defhydra hydra-meghanada-navigate (:color blue :columns 3)
  "Navigate"
  ("g." meghanada-jump-declaration "Goto Declaration")
  ("g/" meghanada-back-jump "Back Jump")
  ("go" meghanada-switch-testcase "Switch Testcase"))

(defhydra hydra-meghanada-compile (:color blue :columns 3)
  "Compile"
  ("f" meghanada-compile-file "File")
  ("p" meghanada-project-compile "Project")
  ("c" meghanada-project-compile "Project"))

(defhydra hydra-meghanada-test (:color blue :columns 3)
  "Test"
  ("t" meghanada-run-junit-class "Class")
  ("c" meghanada-run-junit-class "Class")
  ("f" meghanada-run-junit-class "Class")
  ("T" meghanada-run-junit-test-case "Case")
  ("p" meghanada-run-junit-test-case "Case"))

(+add-mode-command 'hydra-meghanada-mode/body '(meghanada-mode java-mode))
(+add-eval-command 'hydra-meghanada-compile/body '(meghanada-mode java-mode))
(+add-test-command 'hydra-meghanada-test/body '(meghanada-mode java-mode))

(provide 'hydra-meghanada)
;;; hydra-meghanada.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
