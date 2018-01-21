;;; matcha-dired.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
(require 'dired)

(defun matcha-dired-find-file ()
  "Like `find-file' but with `default-directory' set to the
one specified by listing header."
  (interactive)
  (let ((default-directory (dired-current-directory)))
    (call-interactively #'find-file)))

(defhydra matcha-dired-mode (:color blue :hint nil)
  "

    Dired: %s(dired-current-directory)

      Shortcuts               Menu
  ------------------------------------------------------------------------------
    _f_ Find File         _o_ Operate
    _+_ Create Directory  _m_ Marks
    _q_ WDired            _%_ Regexp
                          ^^_i_ Immediate

"
  ("f" matcha-dired-find-file)
  ("+" dired-create-directory)
  ("q" wdired-change-to-wdired-mode)
  ("m" matcha-dired-mark/body)
  ("o" matcha-dired-operate/body)
  ("%" matcha-dired-regexp/body)
  ("i" matcha-dired-immediate/body))

(defhydra matcha-dired-operate (:color blue :hint nil)
  "

    Operate: %s(dired-current-directory)

    Do                       Ownership               Encrypt
  ------------------------------------------------------------------------------
    _d_ Delete            _cw_ Chown          _zz_ Compress (No Encryption)
    _r_ Rename            _cm_ Chmod          _ze_ Encrypt
    _y_ Copy              _cg_ Change Group   _zd_ Decrypt
    _@_ Shell Command     _t_ Touch           _zv_ Verify
    _!_ Async Shell                           ^^_zs_ Sign

    Search                    Misc                  Images
  ------------------------------------------------------------------------------
    _f_ Search            _p_ Print           _id_ Delete Tag
    _ss_ Isearch          _h_ Hardlink        _it_ Tag Files
    _sr_ Isearch Regexp   _l_ Symlink         _ic_ Comment Files
    _q_ Query Replace     _el_ Elisp Load     _is_ Display Thumbs
                          ^^_eb_ Elisp Compile

"
  ("el" dired-do-load)
  ("eb" dired-do-byte-compile)
  ("id" image-dired-delete-tag)
  ("it" image-dired-tag-files)
  ("ic" image-dired-dired-comment-files)
  ("is" image-dired-display-thumbs)
  ("zz" dired-do-compress)
  ("ze" epa-dired-do-encrypt)
  ("zd" epa-dired-do-decrypt)
  ("zv" epa-dired-do-verify)
  ("zs" epa-dired-do-sign)
  ("cw" dired-do-chown)
  ("cg" dired-do-chgrp)
  ("cm" dired-do-chmod)
  ("t" dired-do-touch)
  ("f" dired-do-search)
  ("sr" dired-do-isearch-regexp)
  ("ss" dired-do-isearch)
  ("p" dired-do-print)
  ("h" dired-do-hardlink)
  ("l" dired-do-symlink)
  ("!" dired-do-async-shell-command)
  ("@" dired-do-shell-command)
  ("q" dired-do-query-replace-regexp)
  ("d" dired-do-delete)
  ("r" dired-do-rename)
  ("y" dired-do-copy))

(defhydra matcha-dired-mark (:color blue :hint nil)
  "

    Mark: %s(dired-current-directory)

    Mark                  Flag (For Deletion)      Mark All
  ------------------------------------------------------------------------------
    _m_ Mark            _d_ Mark             _S_ Symlinks
    _u_ Unmark          _g_ Garbage          _D_ Directories
    _U_ Unmark All      _b_ Backup           _X_ Executables
    _T_ Toggle Marks    _s_ Autosave
    _C_ Change Marks    _c_ Clean Directory

    Move
  ------------------------------------------------------------------------------
    _P_ Previous Mark
    _N_ Next Mark

"
  ("P" dired-prev-marked-file)
  ("N" dired-next-marked-file)
  ("S" dired-mark-symlinks)
  ("D" dired-mark-directories)
  ("X" dired-mark-executables)
  ("g" dired-flag-garbage-files)
  ("b" dired-flag-backup-files)
  ("s" dired-flag-auto-save-files)
  ("d" dired-flag-file-deletion)
  ("c" dired-clean-directory)
  ("C" dired-change-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("m" dired-mark)
  ("T" dired-toggle-marks))

(defhydra matcha-dired-regexp (:color blue :hint nil)
  "
    Dired Regexp: %s(dired-current-directory)

    Mark                        Apply                         Link
  ------------------------------------------------------------------------------
    _*_ Mark...               _r_ Rename...             _h_ Hardlink...
    _g_ Mark Containing...    _c_ Copy...               _s_ Symlink...
    _t_ Mark From Image Tag   _d_ Flag for Deletion...
                              ^^_u_ Upcase
                              ^^_l_ Downcase

"
  ("h" dired-do-hardlink-regexp)
  ("s" dired-do-symlink-regexp)
  ("u" dired-upcase)
  ("l" dired-downcase)
  ("r" dired-do-rename-regexp)
  ("c" dired-do-copy-regexp)
  ("d" dired-flag-files-regexp)
  ("t" image-dired-mark-tagged-files)
  ("*" dired-mark-files-regexp)
  ("g" dired-mark-files-containing-regexp))

(defhydra matcha-dired-immediate (:color blue :hint nil)
  "

    Dired Immediate: %s(dired-current-directory)

    Edit Filenames        Open                           Misc
  ------------------------------------------------------------------------------
    _ww_ WDired        _ff_ Find File                _hd_ Hide Details
    _wq_ Quit Wdired   _fp_ Find at Point            _r_ Refresh
                       ^^_fo_ Find in Other Window
                       ^^_fd_ Display in Other Window
                       ^^_fv_ View File in Readonly
                       ^^_d_ Create Directory

       Compare                   Search               Images
  ------------------------------------------------------------------------------
    _cd_ Directories         _ss_ Isearch         _ii_ Display
    _cb_ with Backup         _sr_ Isearch Regexp  _ie_ Display Externally
    _cf_ With another File                        ^^_it_ Toggle Thumbnails

"
  ("ww" wdired-change-to-wdired-mode)
  ("wq" wdired-finish-edit)
  ("ff" find-file)
  ("fv" dired-view-file)
  ("fd" dired-display-file)
  ("fo" dired-find-file-other-window)
  ("fp" dired-find-file)
  ("d" dired-create-directory)
  ("cd" dired-compare-directories)
  ("cb" dired-backup-diff)
  ("cf" dired-diff)
  ("hd" dired-hide-details-mode)
  ("r" revert-buffer)
  ("sr" dired-isearch-filenames-regexp)
  ("ss" dired-isearch-filenames)
  ("ie" image-dired-dired-display-external)
  ("ii" image-dired-dired-display-image)
  ("it" image-dired-dired-toggle-marked-thumbs))

(defun matcha-dired-set-launcher ()
  "Set up `dired' with `hydra'."
  (matcha-set-mode-command :mode 'dired-mode :command #'matcha-dired-mode/body))

(provide 'matcha-dired)
;;; matcha-dired.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
