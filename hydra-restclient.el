;;; hydra-restclient.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
(require 'restclient)

(defhydra hydra-restclient-mode (:color blue)
  "HTTP"
  ("e" restclient-http-send-current "Send Current")
  ("r" restclient-http-send-current-raw "Send Raw")
  ("v" restclient-http-send-current-stay-in-window "Send Current Stay")
  ("n" restclient-jump-next "Jump Next")
  ("p" restclient-jump-prev "Jump Prev")
  ("c" restclient-copy-curl-command "Copy Curl"))

(+add-mode-command #'hydra-restclient-mode/body '(restclient-mode))

(provide 'hydra-restclient)
;;; hydra-restclient.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
