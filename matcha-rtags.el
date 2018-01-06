;;; matcha-rtags.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
;; (require 'rtags)

(defhydra matcha-rtags-print (:color blue :columns 4)
  "Print"
  ("s" rtags-print-symbol-info "Symbol Info")
  ("t" rtags-symbol-type "Symbol Type")
  ("d" rtags-print-dependencies "Dependencies"))

(defhydra matcha-rtags-mode (:color blue :columns 4)
  "RTags"
  ("R" rtags-restart-process "Restart Process")
  ("Q" rtags-quit-rdm "Quit RDM")
  ("f" rtags-fixit "Fixit")
  ("p" matcha-rtags-print/body "Print")
  ("r" rtags-rename-symbol "Rename")
  ("x" rtags-reparse-file "Reparse File")
  ("z" rtags-show-rtags-buffer "Show Rtags Buffer"))

(provide 'matcha-rtags)
;;; matcha-rtags.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
