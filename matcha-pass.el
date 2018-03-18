;;; matcha-pass.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
(require 'password-store nil t)

(defhydra matcha-pass-mode (:color blue :hint nil)
  "

   Password Store:

    Get                     Manage                       Misc
  ------------------------------------------------------------------------------
    _e_ Edit              _i_ Insert Entry           _._ Go To URL
    _l_ Clear Password    _g_ Generate Password      _v_ Pass Version
    _y_ Copy Password     _x_ Remove Entry
                        ^^_r_ Rename Entry

        "
  ("e" password-store-edit)
  ("l" password-store-clear)
  ("y" password-store-copy)
  ("i" password-store-insert)
  ("g" password-store-generate)
  ("x" password-store-remove)
  ("r" password-store-rename)
  ("." password-store-url)
  ("v" password-store-version))

(defun matcha-pass-set-launcher ()
  "Set up `pass' with `hydra'."
  (matcha-set-mode-command :mode 'pass-mode
                           :command #'matcha-pass-mode/body))

(provide 'matcha-pass)
;;; matcha-pass.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
