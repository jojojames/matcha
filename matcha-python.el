;;; matcha-python.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
(require 'python)

(defhydra matcha-python-skelaton (:color blue :hint nil)
  "

    Python Skeleton
  ------------------------------------------------------------------------------
    _i_ If    _f_ For    _w_ While    _t_ Try

    _d_ Def    _I_ Import   _c_ Class

"
  ("i" python-skeleton-if)
  ("f" python-skeleton-for)
  ("w" python-skeleton-while)
  ("t" python-skeleton-try)
  ("c" python-skeleton-class)
  ("d" python-skeleton-def)
  ("I" python-skeleton-import))

(defhydra matcha-python-eval (:color blue :hint nil)
  "

    Python Eval
  ------------------------------------------------------------------------------
    _s_ String    _e_ Function    _r_ Region    _b_ Buffer    _f_ File

"
  ("r" python-shell-send-region)
  ("b" python-shell-send-buffer)
  ("s" python-shell-send-string)
  ("f" python-shell-send-file)
  ("e" python-shell-send-defun))

(defhydra matcha-python-mode (:color blue :hint nil)
  "

    Python
  ------------------------------------------------------------------------------
    _e_ Eval   _z_ Shell    _s_ Skeleton    _p_ Debug    _u_ Run

"
  ("e" matcha-python-eval/body)
  ("z" python-shell-switch-to-shell)
  ("u" run-python)
  ("s" matcha-python-skelaton/body)
  ("p" pdb))

(defun matcha-python-set-launcher ()
  "Set up `hydra' launcher for `python'."
  (matcha-set-mode-command :mode 'python-mode :command #'matcha-python-mode/body)
  (matcha-set-eval-command :mode 'python-mode :command #'matcha-python-eval/body))

(provide 'matcha-python)
;;; matcha-python.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
