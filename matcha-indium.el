(require 'matcha-base)
(require 'matcha-js2-refactor)
;; (require 'indium nil t)
;; (require 'js-import nil t)

(defhydra matcha-js-import (:color blue)
  "Import"
  ("i" js-import "Import")
  ("d" js-import-dev "Import Dev"))

(defhydra matcha-indium-eval (:color blue :columns 4)
  "Eval"
  ("e" indium-eval-last-node "Expression")
  ("f" indium-eval-defun "Function")
  ("i" indium-inspect-last-node "Inspect Last Node")
  ("b" indium-eval-buffer "Buffer"))

(defhydra matcha-indium-debug (:color blue :columns 4)
  "Debug"
  ("b" indium-add-breakpoint "Add Breakpoint")
  ("d" indium-add-breakpoint "Add Breakpoint")
  ("c" indium-add-conditional-breakpoint "Add conditional Breakpoint")
  ("k" indium-remove-breakpoint "Remove Breakpoint")
  ("x" indium-remove-breakpoint "Remove Breakpoint")
  ("K" indium-remove-all-breakpoints-from-buffer "Remove all Breakpoints")
  ("X" indium-remove-all-breakpoints-from-buffer "Remove all Breakpoints")
  ("a" indium-activate-breakpoints "Activate Breakpoints")
  ("D" indium-deactivate-breakpoints "Deactivate Breakpoints")
  ("l" indium-list-breakpoints "List Breakpoints"))

(defhydra matcha-indium-mode (:color blue :columns 4)
  "Javascript"
  ("t" matcha-mocha/body "Test")
  ("i" matcha-js-import/body "Import")
  ("x" indium-run-node "Run Node")
  ("c" indium-scratch "Scratch")
  ("d" matcha-indium-debug/body "Debug")
  ("e" matcha-indium-eval/body "Eval")
  ("r" matcha-js2-refactor/body)
  ("z" indium-switch-to-repl-buffer "Switch to REPL")
  ("k" indium-update-script-source "Update Source"))

(provide 'matcha-indium)
