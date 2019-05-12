;;; matcha-go-mode.el --- Integration with Hydra. -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

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
(require 'go-mode nil t)

(defhydra matcha-go-mode-eval (:color blue :hint nil)
  "
    Go Eval
  ------------------------------------------------------------------------------
   _b_ Play Buffer    _r_ Play Region    _d_ Download Play

"
  ("b" go-play-buffer)
  ("r" go-play-region)
  ("d" go-download-play))

(defhydra matcha-go-mode-goto (:color blue :hint nil)
  "

    Go Goto
  ------------------------------------------------------------------------------
    _a_ Arguments        _i_ Imports
    _d_ Docstring        _r_ Return
    _f_ Function         _m_ Method Receivers
    _n_ Function Name

"
  ("a" go-goto-arguments)
  ("d" go-goto-docstring)
  ("f" go-goto-function)
  ("n" go-goto-function-name)
  ("i" go-goto-imports)
  ("r" go-goto-return-values)
  ("m" go-goto-method-receivers))

(defhydra matcha-go-mode (:color blue :hint nil)
  "

    Go: %s(matcha-projectile-root)

      Do                       Doc                       Misc
  ------------------------------------------------------------------------------
    _=_ Format             _dd_ GoDoc              _m_ Guru
    _e_ Eval               _dp_ GoDoc at Point     _d_ Doctor
    _ia_ Add Import
    _ir_ Remove Imports


    Goto                  Manage
  ------------------------------------------------------------------------------
    _?_ Describe      _c_ Coverage
    _g_ Goto          _ps_ Set Project
    _j_ GoDef Jump    _pR_ Reset GoPath

"
  ("=" gofmt)
  ("dd" godoc)
  ("dp" godoc-at-point)
  ("ia" go-import-add)
  ("e" matcha-go-mode-eval/body)
  ("ir" go-remove-unused-imports)
  ("?" godef-describe)
  ("g" matcha-go-mode-goto/body)
  ("j" godef-jump)
  ("c" go-coverage)
  ("ps" go-set-project)
  ("pR" go-reset-gopath)
  ("m" matcha-go-mode-guru/body)
  ("d" matcha-go-mode-godoctor/body))

(defhydra matcha-go-mode-guru (:color blue :hint nil)
  "

        Go Guru
  ------------------------------------------------------------------------------
    _d_ Describe    _f_ Free Vars    _i_ Implements    _c_ Peers
    _r_ Referrers   _j_ Definition   _p_ Points To     _s_ Callstack
    _e_ Errors      _<_ Callers      _>_ Callees       _x_ Expand Region

"
  ("d" go-guru-describe)
  ("f" go-guru-freevars)
  ("i" go-guru-implements)
  ("c" go-guru-peers)
  ("r" go-guru-referrers)
  ("j" go-guru-definition)
  ("p" go-guru-pointsto)
  ("s" go-guru-callstack)
  ("e" go-guru-whicherrs)
  ("<" go-guru-callers)
  (">" go-guru-callees)
  ("x" go-guru-expand-region))

(defhydra matcha-go-mode-godoctor (:color blue :hint nil)
  "

        Go Doctor
  ------------------------------------------------------------------------------
    _r_ Rename       _R_ Rename (Dry Run)      _S_ Set Scope
    _e_ Extract      _E_ Extract (Dry Run)
    _t_ Toggle       _T_ Toggle (Dry Run)
    _d_ GoDoc        _D_ GoDoc (Dry Run)

"
  ("r" godoctor-rename)
  ("R" godoctor-rename-dry-run)
  ("e" godoctor-extract)
  ("E" godoctor-extract-dry-run)
  ("t" godoctor-toggle)
  ("T" godoctor-toggle-dry-run)
  ("d" godoctor-godoc)
  ("D" godoctor-godoc-dry-run)
  ("S" godoctor-set-scope))

(defun matcha-go-mode-set-launcher ()
  "Set up `go-mode' with `hydra'."
  (matcha-set-refactor-command
   :mode 'go-mode :command 'matcha-go-mode-godoctor/body)
  (matcha-set-eval-command :mode 'go-mode :command 'matcha-go-mode-eval/body)
  (matcha-set-format-command :mode 'go-mode :command 'gofmt)
  (matcha-set-mode-command :mode 'go-mode :command 'matcha-go-mode/body))

(provide 'matcha-go-mode)
;;; matcha-go-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
