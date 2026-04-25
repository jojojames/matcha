;;; matcha-agent-shell.el --- Integration with Transient. -*- lexical-binding: t -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/matcha
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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
(require 'agent-shell nil t)

(defun matcha-agent-shell-set-launcher ()
  "Set up `agent-shell-mode' with `transient'."
  (matcha-set-mode-command
   :mode 'agent-shell-mode :command 'agent-shell-help-menu))

(provide 'matcha-agent-shell)
;;; matcha-agent-shell.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
