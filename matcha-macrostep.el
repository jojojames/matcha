;;; matcha-macrostep.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
(require 'macrostep nil t)

(defun matcha-macrostep-expand-or-open-hydra ()
  "Run `macrostep-expand' if not already. Open hydra otherwise."
  (interactive)
  (if (bound-and-true-p macrostep-mode)
      (matcha-macro-step/body)
    (macrostep-expand)))

(defhydra matcha-macro-step (:color red :hint nil)
  "

   Macrostep: %s(matcha-projectile-root)

     ^^Expand^^               ^^Navigate^^
  ------------------------------------------------------------------------------
    _e_: Expand         _j_: Next Macro
    _c_: Collapse       _k_: Prev Macro
    _C_: Collapse All

"
  ("e" macrostep-expand)
  ("c" macrostep-collapse)
  ("j" macrostep-next-macro)
  ("k" macrostep-prev-macro)
  ("C" macrostep-collapse-all))

(provide 'matcha-macrostep)
;;; matcha-macrostep.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
