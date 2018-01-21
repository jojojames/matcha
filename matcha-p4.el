;;; matcha-p4.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
;; (require 'p4)

;; Unadded
;; p4-group
;; p4-groups
;; p4-jobspec
;; p4-lock
;; p4-login
;; p4-logout
;; p4-passwd
;; p4-set
;; p4-shelve
;; p4-sync --> p4-get
;; p4-tickets
;; p4-unlock
;; p4-unshelve

(defun matcha-p4-dir ()
  "Return root directory of perforce directory.
Fall back to `projectile-project-root'."
  (if-let (root (locate-dominating-file default-directory ".p4config"))
      root
    (projectile-project-root)))

(defhydra matcha-p4 (:color blue :hint nil)
  "

    Perforce: %(matcha-p4-dir)

    Open                 ^^Edit^^              ^^Diff^^
  ------------------------------------------------------------------------------
    _e_ Edit            _E_ Reopen         _=_ Diff Client against Depot
    _K_ Delete          _u_ User           _d_ Diff Files..
    _p_ Print           _S_ Submit         _+_ Diff All Opened
    _@_ Depot Find      _r_ Revert         _-_ Ediff
    _a_ Add             _:_ Label Sync     _V_ Annotate
                        ^^_LL_ Label         _fg_ Grep
                        ^^_j_ Job
                        ^^_b_ Branch
                        ^^_cc_ Change


    Manage                 ^^Info^^             ^^Misc^^
  ------------------------------------------------------------------------------
    _s_ Status          _i_ Info           _h_ Help
    _cl_ Client         _o_ Opened         _G_ Client Name
    _y_ Resolve         _fl_ File Log      _t_ Toggle VC Modeline
    _R_ Refresh         _U_ Users          _w_ Where
    _m_ Move            _FF_ Files         _v_ Version
    _I_ Integrate       _LS_ Labels        _H_ Recently Synced Revisions
    _g_ Update          _D_ Describe       _FL_ Flush Repository
    _C_ Changes         _B_ Branches
    _xx_ Fix            _xs_ Fixes
    _P_ Set Port        _J_ Jobs
                        ^^_FD_ Dump File Info

"
  ("fg" p4-grep)
  ("FD" p4-fstat)
  ("FL" p4-flush)
  ("cc" p4-change)
  ("e" p4-edit)
  ("K" p4-delete)
  ("p" p4-print)
  ("@" p4-depot-find-file)
  ("a" p4-add)
  ("E" p4-reopen)
  ("u" p4-user)
  ("S" p4-submit)
  ("r" p4-revert)
  (":" p4-labelsync)
  ("LL" p4-label)
  ("j" p4-job)
  ("b" p4-branch)
  ("=" p4-diff)
  ("d" p4-diff2)
  ("z" p4-reconcile)
  ("+" p4-diff-all-opened)
  ("-" p4-ediff)
  ("V" p4-annotate)
  ("cl" p4-client)
  ("cL" p4-clients)
  ("y" p4-resolve)
  ("s" p4-status)
  ("R" p4-refresh)
  ("m" p4-move)
  ("I" p4-integ)
  ("g" p4-update)
  ("C" p4-changes)
  ("xx" p4-fix)
  ("xs" p4-fixes)
  ("t" p4-toggle-vc-mode)
  ("P" p4-set-p4-port)
  ("i" p4-info)
  ("w" p4-where)
  ("v" p4-version)
  ("U" p4-users)
  ("o" p4-opened)
  ("LS" p4-labels)
  ("H" p4-have)
  ("h" p4-help)
  ("G" p4-get-client-name)
  ("FF" p4-files)
  ("fl" p4-filelog)
  ("D" p4-describe)
  ("B" p4-branches)
  ("J" p4-jobs))

(provide 'matcha-p4)
;;; matcha-p4.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
