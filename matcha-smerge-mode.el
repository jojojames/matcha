;;; matcha-smerge-mode.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
(require 'smerge-mode)

(defhydra matcha-smerge-mode (:color blue :hint nil)
  "

   Smerge: %(matcha-projectile-root)

    Move               Keep                         Diff
  ------------------------------------------------------------------------------
    _n_ Next        _j_ Current Under Point     _e_ Ediff
    _p_ Previous    _u_ Upper / Mine            _=<_ Base against Upper
                  ^^_l_ Lower / Other           _=>_ Base against Lower
                  ^^_c_ Combine                 _==_ Upper against Lower
                  ^^_b_ Revert to Base
                  ^^_a_ All
    Misc
  ------------------------------------------------------------------------------
    _r_ Automatically Resolve
    _h_ Highlight Conflicts

"
  ("n" smerge-next)
  ("p" smerge-prev)
  ("j" smerge-keep-current)
  ("u" smerge-keep-upper)
  ("l" smerge-keep-lower)
  ("c" smerge-combine-with-next)
  ("b" smerge-keep-base)
  ("a" smerge-keep-all)
  ("r" smerge-resolve)
  ("h" smerge-refine)
  ("e" smerge-ediff)
  ("=<" smerge-diff-base-upper)
  ("=>" smerge-diff-base-lower)
  ("==" smerge-diff-upper-lower))

(defun matcha-smerge-mode-set-launcher ()
  "Set up `hydra' launcher for `smerge-mode'."
  (matcha-set-mode-command :mode 'smerge-mode
                           :command 'matcha-smerge-mode/body
                           :minor-p t))

(provide 'matcha-smerge-mode)
;;; matcha-smerge-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
