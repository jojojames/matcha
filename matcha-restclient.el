;;; matcha-restclient.el --- Integration with Transient. -*- lexical-binding: t -*-

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
(require 'restclient nil t)

(transient-define-prefix matcha-restclient-mode ()
  "Restclient"
  [["Query (Under Cursor)"
    ("e" "Run Query" restclient-http-send-current)
    ("r" "Run Query Raw" restclient-http-send-current-raw)
    ("v" "Run Query (Stay in Window)" restclient-http-send-current-stay-in-window)]
   ["Misc"
    ("m" "Mark Current" restclient-mark-current)
    ("c" "Copy as CURL Command" restclient-copy-curl-command)
    ("n" "Narrow to Current" restclient-narrow-to-current)]
   ["Navigation"
    ("n" "Next" restclient-jump-next)
    ("p" "Previous" restclient-jump-prev)]])

(defun matcha-restclient-set-launcher ()
  "Set `transient' launcher for `restclient'."
  (matcha-set-mode-command :mode 'restclient-mode
                           :command #'matcha-restclient-mode))

(provide 'matcha-restclient)
;;; matcha-restclient.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
