;;; matcha-gud-lldb.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
;; (require 'gud-lldb)

(defhydra matcha-gud-lldb (:color blue :columns 4)
  "LLDB"
  ("d" gud-break "Set breakpoint at current line.")
  ("t" gud-tbreak "Set temporary breakpoint at current line.")
  ("r" gud-remove "Remove breakpoint at current line")
  ("s" gud-step "Step one source line with display.")
  ("i" gud-stepi "Step one instruction with display.")
  ("n" gud-next "Step one line -skip functions-.")
  ("m" gud-nexti "Step one instruction -skip functions-.")
  ("c" gud-cont "Continue with display.")
  ("f" gud-finish "Finish executing current function.")
  ("j" gud-jump "Set execution address to current line.")
  ("<" gud-up "Up N stack frames -numeric arg-.")
  (">" gud-down "Down N stack frames -numeric arg-.")
  ("e" gud-print  "Evaluate C expression at point.")
  ("P" gud-print  "Evaluate C expression at point.")
  ("*" gud-pstar "Evaluate C dereferenced pointer.")
  ("&" gud-pstar "Evaluate C dereferenced pointer.")
  ("u" gud-until "Continue to current line.")
  ("R" gud-run "Run the program.")
  ("v" gud-pv "Print the value of the lisp variable."))

(provide 'matcha-gud-lldb)
;;; matcha-gud-lldb.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
