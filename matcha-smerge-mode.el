;;; matcha-smerge-mode.el --- Integration with Transient. -*- lexical-binding: t -*-

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
(require 'smerge-mode)

(transient-define-prefix matcha-smerge-mode ()
  "Smerge"
  [
   :description (lambda () (format "Smerge: %s" (matcha-projectile-root)))
   ["Navigation"
    ("n" "Next" smerge-next)
    ("p" "Previous" smerge-prev)]
   ["Keep"
    ("j" "Current under Point" smerge-keep-current)
    ("u" "Upper / Mine" smerge-keep-upper)
    ("l" "Lower / Other" smerge-keep-lower)
    ("c" "Combine with Next" smerge-combine-with-next)
    ("b" "Revert to Base" smerge-keep-base)
    ("a" "All" smerge-keep-all)]
   ["Diff"
    ("e" "Ediff" smerge-ediff)
    ("=<" "Base against Upper" smerge-diff-base-upper)
    ("=>" "Base against Lower" smerge-diff-base-lower)
    ("==" "Uper against Lower" smerge-diff-upper-lower)]
   ["Misc"
    ("r" "Automatically Resolve" smerge-resolve)
    ("h" "Highlight Conflicts" smerge-refine)]
   ])

(defun matcha-smerge-mode-set-launcher ()
  "Set up `transient' launcher for `smerge-mode'."
  (matcha-set-mode-command :mode 'smerge-mode
                           :command 'matcha-smerge-mode
                           :minor-p t))

(provide 'matcha-smerge-mode)
;;; matcha-smerge-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
