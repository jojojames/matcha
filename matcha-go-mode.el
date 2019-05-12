;;; matcha-go-mode.el --- Integration with Transient. -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

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
(require 'go-mode nil t)

(define-transient-command matcha-go-mode-eval
  "Eval"
  [["Eval"
    ("b" "Play Buffer" go-play-buffer)
    ("r" "Play Region" go-play-region)
    ("d" "Download Play" go-download-play)]])

(define-transient-command matcha-go-mode-goto
  "Goto"
  [["Goto"
    ("a" "Arguments" go-goto-arguments)
    ("d" "Docstring" go-goto-docstring)
    ("f" "Function" go-goto-function)
    ("n" "Function Name" go-goto-function-name)
    ("i" "Imports" go-goto-imports)
    ("r" "Return Values" go-goto-return-values)
    ("m" "Method Receivers" go-goto-method-receivers)]])

(define-transient-command matcha-go-doc
  "Godoc"
  [["Actions"
    ("d" godoc)
    ("p" godoc-at-point)]])

(define-transient-command matcha-go-mode
  "Go"
  [
   :description (lambda () (format "Go: %s" (matcha-projectile-root)))
   ["Actions"
    ("=" "Format" gofmt)
    ("e" "Eval..." matcha-go-mode-eval)
    ("d" "Godoc..." matcha-go-doc)
    ("m" "Guru..." matcha-go-mode-guru)
    ("d" "GoDoctor..." matcha-go-mode-godoctor)]
   ["References"
    ("j" "GoDef Jump" godef-jump)
    ("?" "Describe" godef-describe)
    ("g" "Goto..."matcha-go-mode-goto)]
   ["Imports"
    ("ia" "Add" go-import-add)
    ("ir" "Remove Unused" go-remove-unused-imports)]
   ["Misc"
    ("c" "Coverage" go-coverage)
    ("ps" "Set Project" go-set-project)
    ("pR" "Reset GoPath" go-reset-gopath)]])

(define-transient-command matcha-go-mode-guru
  "Guru"
  [[("d" "Describe" go-guru-describe)
    ("f" "Free Vars" go-guru-freevars)
    ("i" "Implements" go-guru-implements)
    ("c" "Peers" go-guru-peers)]
   [("r" "Referrers" go-guru-referrers)
    ("j" "Definition" go-guru-definition)
    ("p" "Points To" go-guru-pointsto)
    ("s" "Callstack" go-guru-callstack)]
   [("e" "Which Errs" go-guru-whicherrs)
    ("<" "Callers" go-guru-callers)
    (">" "Callees" go-guru-callees)
    ("x" "Expand Region" go-guru-expand-region)]])

(define-transient-command matcha-go-mode-godoctor
  "Doctor"
  ;; FIXME: It might be interesting to use an `transient' argument to toggle dry run.
  [["Doctor"
    ("r" godoctor-rename)
    ("e" godoctor-extract)
    ("t" godoctor-toggle)
    ("d" godoctor-godoc)]
   ["DryRun Doctor"
    ("R" godoctor-rename-dry-run)
    ("E" godoctor-extract-dry-run)
    ("T" godoctor-toggle-dry-run)
    ("D" godoctor-godoc-dry-run)]
   ["Manage"
    ("S" godoctor-set-scope)]])

(defun matcha-go-mode-set-launcher ()
  "Set up `go-mode' with `transient'."
  (matcha-set-refactor-command
   :mode 'go-mode :command 'matcha-go-mode-godoctor)
  (matcha-set-eval-command :mode 'go-mode :command 'matcha-go-mode-eval)
  (matcha-set-format-command :mode 'go-mode :command 'gofmt)
  (matcha-set-mode-command :mode 'go-mode :command 'matcha-go-mode))

(provide 'matcha-go-mode)
;;; matcha-go-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
