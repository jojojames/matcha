;;; hydra-python.el --- Integration with Hydra. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/hydra-integrations
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: hydra, emacs
;; HomePage: https://github.com/jojojames/hydra-integrations

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
(require 'hydra-integration-base)
(require 'python)

(defhydra hydra-python-skelaton (:color blue :columns 4)
  "Skeleton"
  ("c" python-skeleton-class "Class")
  ("d" python-skeleton-def "Def")
  ("f" python-skeleton-for "For")
  ("i" python-skeleton-if "If")
  ("I" python-skeleton-import "Import")
  ("t" python-skeleton-try "Try")
  ("w" python-skeleton-while "While"))

(defhydra hydra-python-eval (:color blue)
  "Python Eval"
  ("r" python-shell-send-region "Send Region")
  ("b" python-shell-send-buffer "Send Buffer")
  ("s" python-shell-send-string "Send String")
  ("l" python-shell-send-file "Send File")
  ("e" python-shell-send-defun "Send Function"))

(defhydra hydra-python-mode (:color blue)
  "Python"
  ("e" hydra-python-eval/body "Eval")
  ("z" python-shell-switch-to-shell "Shell")
  ("u" run-python "Run")
  ("s" hydra-python-skelaton/body "Skeleton")
  ("p" pdb "Pdb"))

(+add-mode-command #'hydra-python-mode/body '(python-mode))
(+add-mode-command #'hydra-python-eval/body '(python-mode))

(provide 'hydra-python)
;;; hydra-python.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
