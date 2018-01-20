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

(defhydra matcha-go-mode-goto (:color blue :hint nil)
  "

    Go Goto
  ------------------------------------------------------------------------------
    [_a_] Arguments        [_i_] Imports
    [_d_] Docstring        [_r_] Return
    [_f_] Function         [_m_] Method Receivers
    [_n_] Function Name

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

      Do                       Doc                  Play
  ------------------------------------------------------------------------------
    [_=_] Format           [_dd_] GoDoc            [_pb_] Play Buffer
    [_ia_] Add Import      [_dp_] GoDoc at Point   [_pr_] Play Region
    [_ir_] Remove Imports                        ^^[_pd_] Download Play

    Goto                  Manage
  ------------------------------------------------------------------------------
    [_?_] Describe      [_c_] Coverage
    [_g_] Goto          [_ps_] Set Project
    [_j_] GoDef Jump    [_pR_] Reset GoPath

"
  ("=" gofmt)
  ("dd" godoc)
  ("dp" godoc-at-point)
  ("ia" go-import-add)
  ("ir" go-remove-unused-imports)
  ("pb" go-play-buffer)
  ("pr" go-play-region)
  ("pd" go-download-play)
  ("?" godef-describe)
  ("g" matcha-go-mode-goto/body)
  ("j" godef-jump)
  ("c" go-coverage)
  ("ps" go-set-project)
  ("pR" go-reset-gopath))

(defun matcha-go-mode-set-launcher ()
  "Set up `go-mode' with `hydra'."
  (+add-major-indent-command 'gofmt '(go-mode))
  (+add-major-mode-command 'matcha-go-mode/body '(go-mode)))

(provide 'matcha-go-mode)
;;; matcha-go-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
