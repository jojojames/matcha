;;; matcha-erlang.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
;; (require 'erlang)

(defhydra matcha-erlang-debug (:color blue)
  "Debug"
  ("i" edb-toggle-interpret "Toggle Interpret")
  ("b" edb-toggle-breakpoint "Toggle Breakpoint")
  ("d" edb-toggle-breakpoint "Toggle Breakpoint")
  ("s" edb-synch-breakpoints "Sync Breakpoints")
  ("S" edb-save-dbg-state" Save Debug State")
  ("R" edb-restore-dbg-state "Restore Debug State")
  ("m" edb-monitor "Monitor"))

(defhydra matcha-erlang-node (:color blue)
  "Node Communication"
  ("n" erl-choose-nodename "Choose Nodename")
  ("p" erl-ping "Check Connection")
  ("P" erl-process-list "List Processes")
  ("r" erl-reload-module "Reload an Erlang Module")
  ("R" erl-reload-modules "Reload all Erlang Modules")
  ("w" erl-who-calls "Who Calls Function?")
  ("W" erl-rebuild-callgraph "Rebuild Who Calls Graph"))

(defhydra matcha-erlang-eval (:color blue)
  "Eval"
  ("e" erl-eval-expression "Eval Expression")
  ("z" erl-eval-expression "Create Interactive Session"))

(defhydra matcha-erlang-help (:color blue)
  "Help"
  ("s" erl-find-sig-under-point "Find Sig Under Point")
  ("S" erl-find-sig "Find Sig")
  ("k" erl-find-doc-under-point "Find Doc Under Point")
  ("K" erl-find-doc "Find Doc")
  ("d" erl-fdoc-describe "Describe")
  ("a" erl-fdoc-apropos "Apropos")
  ("f" fprof "Fprof")
  ("F" fprof-analyse "Fprof Analyze"))

(defhydra matcha-erlang-mode (:color blue)
  "Erlang"
  ("e" matcha-erlang-eval/body "Eval")
  ("d" matcha-erlang-debug/body "Debug")
  ("h" matcha-erlang-help/body "Help")
  ("c" matcha-erlang-node/body "Node Communication")
  ("r" erl-refactor-subfunction "Refactor Subfunction")
  ("l" erlang-compile-display "Compile Display")
  ("u" erlang-compile "Compile")
  ("z" erlang-shell-display "Shell"))

(defun matcha-erlang-set-launcher ()
  "Set up `hydra' launcher for `erlang'."
  (matcha-set-mode-command
   :mode 'erlang-mode :command #'matcha-erlang-mode/body)
  (matcha-set-eval-command
   :mode 'erlang-mode :command #'matcha-erlang-eval/body)
  (matcha-set-debug-command
   :mode 'erlang-mode :command #'matcha-erlang-debug/body))

(provide 'matcha-erlang)
;;; matcha-erlang.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
