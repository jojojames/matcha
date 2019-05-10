;;; matcha-kotlin-mode.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
(require 'kotlin-mode nil t)

(defhydra matcha-kotlin-mode-eval (:color blue :hint nil)
  "

    Kotlin Evaluation
  ------------------------------------------------------------------------------
    _e_ Line    _r_ Region    _k_ Block    _b_ Buffer

"
  ("e" kotlin-send-line)
  ("r" kotlin-send-region)
  ("k" kotlin-send-block)
  ("b" kotlin-send-buffer))

(defhydra matcha-kotlin-mode (:color blue :hint nil)
  "

    Kotlin: %s(matcha-projectile-root)
  ------------------------------------------------------------------------------
    _z_ REPL    _e_ Eval

"
  ("z" kotlin-repl)
  ("e" matcha-kotlin-mode-eval/body))

(defun matcha-kotlin-mode-set-launcher ()
  "Set up `kotlin-mode' with `hydra'."
  (matcha-set-mode-command :mode 'kotlin-mode
                           :command 'matcha-kotlin-mode/body)
  (matcha-set-eval-command :mode 'kotlin-mode
                           :command 'matcha-kotlin-mode-eval/body))

(provide 'matcha-kotlin-mode)
;;; matcha-kotlin-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
