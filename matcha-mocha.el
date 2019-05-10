;;; matcha-mocha.el --- Integration with Transient. -*- lexical-binding: t -*-

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
(require 'matcha-base)
(require 'mocha nil t)

(define-transient-command matcha-mocha
  "Mocha"
  [["Test"
    ("f" "File" mocha-test-file)
    ("t" "At Point" mocha-test-at-point)
    ("p" "Project" mocha-test-project)]
   ["Debug"
    ("df" "File" mocha-debug-file)
    ("dt" "At Point" mocha-debug-at-point)
    ("dp" "Project" mocha-debug-project)]])

(provide 'matcha-mocha)
;;; matcha-mocha.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
