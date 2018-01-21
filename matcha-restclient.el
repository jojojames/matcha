;;; matcha-restclient.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
(require 'restclient nil t)

(defhydra matcha-restclient-mode (:color blue :hint nil)
  "

    Restclient:

    Query (Under Cursor)                 Navigation
  ------------------------------------------------------------------------------
    _e_ Run Query                     _n_ Next
    _r_ Run Query Raw                 _p_ Previous
    _v_ Run Query (Stay in Window)

    Misc
  ------------------------------------------------------------------------------
    _m_ Mark Query
    _c_ Copy as CURL Command
    _n_ Narrow to Current

"
  ("e" restclient-http-send-current)
  ("r" restclient-http-send-current-raw)
  ("v" restclient-http-send-current-stay-in-window)
  ("n" restclient-jump-next)
  ("p" restclient-jump-prev)
  ("m" restclient-mark-current)
  ("c" restclient-copy-curl-command)
  ("n" restclient-narrow-to-current))

(defun matcha-restclient-set-launcher ()
  "Set `hydra' launcher for `restclient'."
  (matcha-set-mode-command :mode 'restclient-mode
                           :command #'matcha-restclient-mode/body))

(provide 'matcha-restclient)
;;; matcha-restclient.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
