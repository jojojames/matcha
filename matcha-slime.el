;;; matcha-slime.el --- Integration with Transient. -*- lexical-binding: t -*-

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
(require 'slime nil t)
(require 'slime-repl nil t)
(require 'slime-scratch nil t)

;; TODO: Add Debug Transient

(transient-define-prefix matcha-slime-mode-eval ()
  "Eval"
  [["Eval"
    ("e" "Expression" slime-eval-last-expression)
    ("r" "Region" slime-eval-region)
    ("b" "Buffer" slime-eval-buffer)
    ("d" "Defun" slime-eval-defun)
    ("v" "Defvar" slime-re-evaluate-defvar)
    ("i" "Interactively" slime-interactive-eval)
    ("E" "Edit Value" slime-edit-value)]
   ["Eval & Print"
    ("j" "Expression" slime-eval-print-last-expression)
    ("R" "Region" slime-pprint-eval-region)
    ("J" "Pprint" slime-pprint-eval-last-expression)
    ("I" "Eval & Inspect" slime-inspect)]
   ["Compile"
    ("L" "Load File" slime-load-file)
    ("cc" "File" slime-compile-file)
    ("cl" "Compile & Load File" slime-compile-and-load-file)
    ("cd" "Defun" slime-compile-defun)
    ("cr" "Region" slime-compile-region)]
   ["Misc"
    ("l" "Eval in REPL" slime-eval-last-expression-in-repl)
    ("u" "Undefine Function" slime-undefine-function)
    ("n" "Remove Notes" slime-remove-notes)
    ("i" "Interrupt" slime-interrupt)]])

(transient-define-prefix matcha-slime-mode-connection ()
  "Connections"
  [["Connections"
    ("n" "Next Connection" slime-next-connection)
    ("p" "Previous Connection" slime-prev-connection)
    ("l" "List Connections" slime-list-connections)]])

(transient-define-prefix matcha-slime-mode-info ()
  "Info"
  [["Apropos"
    ("a" "Apropos" slime-apropos)
    ("l" "Apropos All" slime-apropos-all)
    ("p" "Apropos Package" slime-apropos-package)]
   ["Misc"
    ("h" "Describe Symbol" slime-describe-symbol)
    ("d" "Disassemble Symbol" slime-disassemble-symbol)
    ("H" "Hyperspec Lookup" slime-hyperspec-lookup)]])

(transient-define-prefix matcha-slime-mode-references ()
  "References"
  [["Who"
    ("r" "References" slime-who-references)
    ("m" "Macroexpands" slime-who-macroexpands)
    ("l" "Specializes" slime-who-specializes)
    ("c" "Calls" slime-who-calls)
    ("w" "Calls Who" slime-calls-who)
    ("b" "Binds" slime-who-binds)
    ("s" "Sets" slime-who-sets)]
   ["List"
    ("C" "Callers" slime-list-callers)
    ("E" "Callees" slime-list-callees)]])

(transient-define-prefix matcha-slime-mode ()
  "SLIME"
  [["Actions"
    ("e" "Eval..." matcha-slime-mode-eval)
    ("w" "References..." matcha-slime-mode-references)
    ("h" "Info..." matcha-slime-mode-info)
    ("l" "Connections..." matcha-slime-mode-connection)]
   ["Misc"
    ("x" "*scratch*" slime-scratch)
    ("z" "REPL" slime-repl-mode)
    ("S" "SLIME" slime)
    ("Q" "Quit Lisp" slime-quit-lisp)
    ("?" "Cheat Sheet" slime-cheat-sheet)]
   ["Macroexpand & Trace"
    ("mo" "Macroexpand" slime-macroexpand-1)
    ("ma" "Macroexpand All" slime-macroexpand-all)
    ("L" "List Threads" slime-list-threads)
    ("Af" "Toggle Trace" slime-toggle-trace-fdefinition)
    ("Au" "Untrace All" slime-untrace-all)]])

(defun matcha-slime-set-launcher ()
  "Set `transient' launcher for `slime'."
  (matcha-set-mode-command
   :mode 'slime-mode :command #'matcha-slime-mode :minor-p t)
  (matcha-set-eval-command
   :mode 'slime-mode :command #'matcha-slime-eval :minor-p t))

(provide 'matcha-slime)
;;; matcha-slime.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
