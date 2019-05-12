;;; matcha-python.el --- Integration with Transient. -*- lexical-binding: t -*-

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
(require 'python)

(declare-function 'lispy-eval "lispy")
(declare-function 'lispy-eval-and-insert "lispy")

(define-transient-command matcha-python-skeleton
  "Skeleton"
  [["Template"
    ("i" "If" python-skeleton-if)
    ("f" "For" python-skeleton-for)
    ("w" "While" python-skeleton-while)
    ("t" "Try" python-skeleton-try)
    ("c" "Class" python-skeleton-class)
    ("d" "Def" python-skeleton-def)
    ("I" "Import" python-skeleton-import)]])

(define-transient-command matcha-python-eval
  "Eval"
  [["Send"
    ("r" "Region" python-shell-send-region)
    ("b" "Buffer" python-shell-send-buffer)
    ("s" "String" python-shell-send-string)
    ("f" "File" python-shell-send-file)
    ("d" "Function" python-shell-send-defun)]
   ["Lispy"
    ;; FIXME: Figure out how to add this dynamically.
    ;; (e.g. Only if Lispy is active.)
    ("e" lispy-eval)
    ("i" lispy-eval-and-insert)]])

(define-transient-command matcha-python-mode
  "Python"
  [["Actions"
    ("z" "Switch to Shell" python-shell-switch-to-shell)
    ("u" "Run Python" run-python)
    ("p" "Debug" pdb)
    ("e" "Eval..." matcha-python-eval)
    ("s" "Skeleton..." matcha-python-skeleton)]])

(defun matcha-python-set-launcher ()
  "Set up `transient' launcher for `python'."
  (matcha-set-mode-command :mode 'python-mode :command #'matcha-python-mode)
  (matcha-set-eval-command :mode 'python-mode :command #'matcha-python-eval))

(provide 'matcha-python)
;;; matcha-python.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
