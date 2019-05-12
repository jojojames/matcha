;;; matcha-kotlin-mode.el --- Integration with Transient. -*- lexical-binding: t -*-

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
(require 'kotlin-mode nil t)

(define-transient-command matcha-kotlin-mode-eval
  "Eval"
  [["Send"
    ("e" "Line" kotlin-send-line)
    ("r" "Region" kotlin-send-region)
    ("k" "Block" kotlin-send-block)
    ("b" "Buffer" kotlin-send-buffer)]])

(define-transient-command matcha-kotlin-mode
  "Kotlin"
  [["Actions"
    ("z" "REPL" kotlin-repl)
    ("e" "Eval..." matcha-kotlin-mode-eval)]])

(defun matcha-kotlin-mode-set-launcher ()
  "Set up `kotlin-mode' with `transient'."
  (matcha-set-mode-command :mode 'kotlin-mode
                           :command 'matcha-kotlin-mode)
  (matcha-set-eval-command :mode 'kotlin-mode
                           :command 'matcha-kotlin-mode-eval))

(provide 'matcha-kotlin-mode)
;;; matcha-kotlin-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
