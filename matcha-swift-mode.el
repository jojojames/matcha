;;; matcha-swift-mode.el --- Integration with Transient. -*- lexical-binding: t -*-

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
(require 'swift-mode nil t)

(define-transient-command matcha-swift-mode
  "Swift"
  [["Actions"
    ("z" "Run REPL" swift-mode:run-repl)
    ("b" "Send Buffer" swift-mode:send-buffer)
    ("r" "Send Region" swift-mode:send-region)]])

(defun matcha-swift-mode-set-launcher ()
  "Set up `swift-mode' with `transient'."
  (matcha-set-mode-command :mode 'swift-mode
                           :command #'matcha-swift-mode))

(provide 'matcha-swift-mode)
;;; matcha-swift-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
