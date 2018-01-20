;;; matcha-tide.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
(require 'matcha-mocha)
(require 'matcha-prettier)
;; (require 'tide nil t)
;; (require 'ts-comint)

(defhydra matcha-tide-eval (:color blue :columns 4)
  "Typescript Eval"
  ("z" run-ts "Run REPL")
  ("e" ts-send-last-sexp "Send S-exp")
  ("x" ts-send-last-sexp-and-go "Send Last S-exp and Go")
  ("b" ts-send-buffer "Send Buffer")
  ("v" ts-send-buffer-and-go "Send Buffer and Go")
  ("l" ts-load-file-and-go "Load File and Go"))

(defhydra matcha-tide-mode (:color blue :hint nil)
  "

   Typescript: %(matcha-projectile-root)

    ^Do^           ^Refactor^             ^Info^                  ^Misc^
  ------------------------------------------------------------------------------
    [_t_] Test    [_n_] Rename       [_k_] Show Doc           [_z_] Restart Server
    [_e_] Eval    [_f_] Apply Fix    [_?_] Find References    [_E_] Errors
                ^^[_=_] Format       [_._] GoTo Definition    [_j_] Add JSDoc
                ^^[_p_] Prettier     [_/_] Pop Definition
                ^^[_r_] Refactor

"
  ("p" matcha-prettier-or-indent-region-or-buffer)
  ("t" matcha-mocha/body)
  ("e" matcha-tide-eval/body)
  ("n" tide-rename-symbol)
  ("r" tide-refactor)
  ("=" tide-format)
  ("f" tide-fix)
  ("z" tide-restart-server)
  ("E" tide-project-errors)
  ("k" tide-documentation-at-point)
  ("?" tide-references)
  ("." tide-jump-to-definition)
  ("/" tide-jump-back)
  ("j" tide-jsdoc-template))

(defun matcha-tide-set-launcher ()
  "Set `hydra' launcher for `tide'."
  (matcha-set-test-command :mode 'tide-mode
                           :command #'matcha-mocha/body
                           :minor-p t)

  (matcha-set-eval-command :mode 'tide-mode
                           :command #'matcha-tide-eval/body
                           :minor-p t)

  (matcha-set-format-command :mode '(tide-mode)
                             :command #'tide-format
                             :minor-p t)

  (matcha-set-mode-command :mode 'tide-mode
                           :command #'matcha-tide-mode/body
                           :minor-p t))

(provide 'matcha-tide)
;;; matcha-tide.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
