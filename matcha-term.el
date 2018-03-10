;;; matcha-term.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
(require 'term)

(defhydra matcha-term (:color blue :hint nil)
  "

    Term
  ------------------------------------------------------------------------------
    _c_ Char Mode    _l_ Line Mode

"
  ("c" term-char-mode)
  ("m" term-char-mode)
  ("l" term-line-mode)
  ("j" term-line-mode)
  ("k" term-char-mode))

(defun matcha-term-set-launcher ()
  "Set `hydra' launcher for `term'."
  (matcha-set-mode-command :mode 'term-mode :command #'matcha-term/body))

(provide 'matcha-term)
;;; matcha-term.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
