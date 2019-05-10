;;; matcha-tide.el --- Integration with Transient. -*- lexical-binding: t -*-

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
(require 'matcha-mocha)
(require 'matcha-prettier)
(require 'tide nil t)
;; (require 'ts-comint)

(define-transient-command matcha-tide-refactor
  "Refactor"
  [["Actions"
    ("n" "Rename" tide-rename-symbol)
    ("r" "Refactor" tide-refactor)
    ("f" "Fix" tide-fix)]])

(define-transient-command matcha-tide-eval
  "Eval"
  [["Send"
    ("e" "Sexp" ts-send-last-sexp)
    ("x" "Last Sexp and Go" ts-send-last-sexp-and-go)
    ("b" "Buffer" ts-send-buffer)
    ("v" "Buffer and Go" ts-send-buffer-and-go)]
   ["Misc"
    ("l" "Load File and Go" ts-load-file-and-go)]])

(define-transient-command matcha-tide-mode
  "Typescript"
  [["Actions"
    ("e" "Eval..." matcha-tide-eval)
    ("t" "Test..." matcha-mocha)
    ("r" "Refactor..." matcha-tide-refactor)
    ("=" "Format" tide-format)
    ("p" "Prettier" matcha-prettier-or-indent-region-or-buffer)
    ("z" "Run REPL" run-ts)]
   ["Info"
    ("k" "Documentation" tide-documentation-at-point)
    ("?" "Find References" tide-references)
    ("." "GoTo Definition" tide-jump-to-definition)
    ("/" "Pop Definition" tide-jump-back)]
   ["Misc"
    ("z" "Restart Server" tide-restart-server)
    ("E" "Project Errors" tide-project-errors)
    ("j" "JSDoc Template" tide-jsdoc-template)]])

(defun matcha-tide-set-launcher ()
  "Set `transient' launcher for `tide'."
  (matcha-set-refactor-command
   :mode 'tide-mode :command #'matcha-tide-refactor :minor-p t)
  (matcha-set-test-command
   :mode 'tide-mode :command #'matcha-mocha :minor-p t)
  (matcha-set-eval-command
   :mode 'tide-mode :command #'matcha-tide-eval :minor-p t)
  (matcha-set-format-command
   :mode 'tide-mode :command #'tide-format :minor-p t)
  (matcha-set-mode-command
   :mode 'tide-mode :command #'matcha-tide-mode :minor-p t))

(provide 'matcha-tide)
;;; matcha-tide.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
