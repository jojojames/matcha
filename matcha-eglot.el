;;; matcha-eglot.el --- Integration with Transient. -*- lexical-binding: t -*-

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
(require 'eglot nil t)

(define-transient-command matcha-eglot
  "Eglot"
  [["Find"
    ("d" "Declaration" eglot-find-declaration)
    ("i" "Implementation" eglot-find-implementation)
    ("D" "Type Definition" eglot-find-typeDefinition)]
   ["Edit"
    ("r" "Rename" eglot-rename)
    ("a" "Code Actions" eglot-code-actions)
    ("=" "Format Buffer" eglot-format-buffer)
    ("R" "Format Region" eglot-format)]
   ["Manage"
    ("X" "Shutdown" eglot-shutdown)
    ("C" "Reconnect" eglot-reconnect)
    ("E" "Display Events Buffer" eglot-events-buffer)]])

(define-transient-command matcha-eglot-format
  "Eglot Format"
  [["Format"
    ("=" "Format Buffer" eglot-format-buffer)
    ("R" "Format Region" eglot-format)]])

(define-transient-command matcha-eglot-refactor
  "Eglot Refactor"
  [["Refactor"
    ("r" "Rename" eglot-rename)
    ("a" "Code Actions" eglot-code-actions)]])

(defun matcha-eglot-set-launcher ()
  "Set `transient' launcher for `eglot'."
  (matcha-set-refactor-command
   :mode 'eglot-mode :command #'matcha-eglot-refactor :minor-p t)
  (matcha-set-format-command
   :mode 'eglot-mode :command #'matcha-eglot-format :minor-p t)
  (matcha-set-mode-command
   :mode 'eglot-mode :command #'matcha-eglot :minor-p t))

(provide 'matcha-eglot)
;;; matcha-eglot.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
