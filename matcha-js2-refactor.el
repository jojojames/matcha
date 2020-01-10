;;; matcha-js2-refactor.el --- Integration with Transient. -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

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
;; (require 'js2-refactor nil t)

(define-transient-command matcha-js2-refactor
  "JS2 Refactor"
  [
   :description (lambda () (format "Javascript Refactor: %s" (matcha-heading-current-file)))
   ["Functions"
    ("ef" "Extract Function" js2r-extract-function)
    ("em" "Extract Method" js2r-extract-method)
    ("ip" "Introduce parameter" js2r-introduce-parameter)
    ("lp" "Localize parameter" js2r-localize-parameter)
    ("ao" "Arguments to object" js2r-arguments-to-object)
    ("tf" "Toggle fun expr and decl" js2r-toggle-function-expression-and-declaration)
    ("ta" "Toggle fun expr and =>" js2r-toggle-arrow-function-and-expression)
    ("ts" "Toggle fun async" js2r-toggle-function-async)]
   ["Variables"
    ("ee" "Expand node" js2r-expand-node-at-point)
    ("cc" "Contract node" js2r-contract-node-at-point)
    ("ev" "Extract var" js2r-extract-var)
    ("el" "Extract let" js2r-extract-let)
    ("ec" "Extract const" js2r-extract-const)
    ("iv" "Inline variable" js2r-inline-var)
    ("rv" "Rename variable" js2r-rename-var)
    ("vt" "var to this" js2r-var-to-this)
    ("sv" "Split var decleration" js2r-split-var-declaration)]
   ["Expressions"
    ("ti" "Ternary to if" js2r-ternary-to-if)
    ("wl" "Wrap in for loop" js2r-wrap-in-for-loop)
    ("ss" "Split string" js2r-split-string)
    ("st" "String to template" js2r-string-to-template)
    ("uw" "Unwrap" js2r-unwrap)
    ("k" "JS2 Kill" js2r-kill)
    ("sl" "Forward slurp" js2r-forward-slurp)
    ("ba" "Forward barf" js2r-forward-barf)]
   ["File / Debug"
    ("wi" "Wrap buffer in iife" js2r-wrap-buffer-in-iife)
    ("ig" "Inject global in iife" js2r-inject-global-in-iife)
    ("ag" "Add to globals annotation" js2r-add-to-globals-annotation)
    ("lt" "Log this" js2r-log-this)
    ("dt" "Debug this" js2r-debug-this)]])

(provide 'matcha-js2-refactor)
;;; matcha-js2-refactor.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
