;;; matcha-evil-mc.el --- Integration with Hydra. -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

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
(require 'evil-mc nil t)

;; https://github.com/gabesoft/evil-mc/issues/22
(defun col-at-point (point)
  (save-excursion (goto-char point) (current-column)))

(defun evil--mc-make-cursor-at-col-append (_startcol endcol orig-line)
  (end-of-line)
  (when (> endcol (current-column))
    (insert-char ?\s (- endcol (current-column))))
  (move-to-column (- endcol 1))
  (unless (= (line-number-at-pos) orig-line)
    (evil-mc-make-cursor-here)))

(defun evil--mc-make-cursor-at-col-insert (startcol _endcol orig-line)
  (end-of-line)
  (move-to-column startcol)
  (unless (or (= (line-number-at-pos) orig-line) (> startcol (current-column)))
    (evil-mc-make-cursor-here)))

(defun evil--mc-make-vertical-cursors (beg end func)
  (evil-mc-pause-cursors)
  (apply-on-rectangle func
                      beg end (line-number-at-pos (point)))
  (evil-mc-resume-cursors)
  (evil-normal-state))

(defun evil-mc-insert-vertical-cursors (beg end)
  (interactive (list (region-beginning) (region-end)))
  (evil--mc-make-vertical-cursors beg end 'evil--mc-make-cursor-at-col-insert)
  (move-to-column (min (col-at-point beg) (col-at-point end))))

(defun evil-mc-append-vertical-cursors (beg end)
  (interactive (list (region-beginning) (region-end)))
  (evil--mc-make-vertical-cursors beg end 'evil--mc-make-cursor-at-col-append)
  (move-to-column (- (max (col-at-point beg) (col-at-point end)) 1)))

(defun evil-mc-make-cursors-in-defun ()
  "Like `evil-mc-make-all-cursors' but only in the current defun."
  (interactive)
  (if (evil-mc-has-cursors-p)
      (evil-mc-undo-all-cursors)
    (let ((window-start (window-start)))
      (save-restriction
        (widen)
        (narrow-to-defun)
        (evil-mc-make-all-cursors))
      (set-window-start nil window-start))))

(defhydra matcha-evil-mc (:color red :hint nil)
  "

    Multiple Cursors:
  ------------------------------------------------------------------------------
    _m_ Make all Cursors    _u_ Undo all Cursors
    _s_ Pause Cursors       _r_ Resume Cursors

    _f_ Make & Goto First Cursor  _l_ Make & Goto Last Cursor
    _h_ Make Here
    _j_ Make & Goto Next Line     _k_ Make & Goto Prev Line

    _n_ Make & Goto Next Cursor   _p_ Make & Goto Previous Cursor
    _S_ Skip & Goto Next Match
    _N_ Make & Goto Next Match    _P_ Make & Goto Previous Match

    _=_ Make in Defun
    _<_ Insert Vertical Cursors   _>_ Append Vertical Cursors

"
  ("=" evil-mc-make-cursors-in-defun)
  ("<" evil-mc-insert-vertical-cursors)
  (">" evil-mc-append-vertical-cursors)
  ("m" evil-mc-make-all-cursors)
  ("u" evil-mc-undo-all-cursors)
  ("s" evil-mc-pause-cursors)
  ("r" evil-mc-resume-cursors)
  ("f" evil-mc-make-and-goto-first-cursor)
  ("l" evil-mc-make-and-goto-last-cursor)
  ("h" evil-mc-make-cursor-here)
  ("j" evil-mc-make-cursor-move-next-line)
  ("k" evil-mc-make-cursor-move-prev-line)
  ("n" evil-mc-make-and-goto-next-cursor)
  ("p" evil-mc-make-and-goto-prev-cursor)
  ("S" evil-mc-skip-and-goto-next-match)
  ("N" evil-mc-make-and-goto-next-match)
  ("P" evil-mc-make-and-goto-prev-match))

(provide 'matcha-evil-mc)
;;; matcha-evil-mc.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
