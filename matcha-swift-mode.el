;;; matcha-swift-mode.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
;; (require 'swift-mode)

(defhydra matcha-swift-mode (:color blue :columns 4)
  "Swift"
  ("ez" swift-mode:run-repl "Run REPL")
  ("eb" swift-mode:send-buffer "Send Buffer")
  ("er" swift-mode:send-region "Send Region"))

(defun matcha-swift-mode-set-launcher ()
  "Set up `swift-mode' with `hydra'."
  (matcha-set-mode-command :mode 'swift-mode
                           :command #'matcha-swift-mode/body))

(provide 'matcha-swift-mode)
;;; matcha-swift-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
