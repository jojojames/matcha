;;; hydra-org.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
(require 'org)

(defhydra hydra-org-babel (:color blue :columns 4)
  "Babel"
  ("e" org-babel-execute-src-block "Execute Source Block")
  ("'" org-edit-src-code "Edit Source"))

(defhydra hydra-org-hyperlink (:color blue :columns 4)
  "Links"
  ("l" org-store-link "Store Link")
  ("s" org-store-link "Store Link")
  ("r" org-occur-link-in-agenda-files "Occur Links")
  ("i" org-insert-link "Insert Link")
  ("g" org-open-at-point "Follow Link")
  ("n" org-next-link "Next Link")
  ("p" org-previous-link "Previous Link")
  ("t" org-toggle-link-display "Toggle Link Display")
  ("d" org-toggle-link-display "Toggle Link Display"))

(defhydra hydra-org-time (:color blue :columns 4)
  "Time"
  ("t" org-time-stamp "Timestamp")
  ("T" org-time-stamp-inactive "Inactive Timestamp")
  ("D" hydra-org-change-date/body "Change Date")
  ("y" org-evaluate-time-range "Evaluate Time Range")
  ("s" org-schedule "Schedule Item")
  ("d" org-deadline "Deadline")
  ("Z" org-toggle-time-stamp-overlays "Custom Time Format")
  ("c" org-goto-calendar "Goto Calendar")
  ("C" org-date-from-calendar "Date from Calendar")
  ("0" org-timer-start "Start Timer")
  ("9" org-timer-pause-or-continue "Pause/Continue Timer")
  ("8" org-timer-pause-or-continue "Pause/Continue Timer")
  ("7" org-timer "Insert Timer String")
  ("6" org-timer-item "Insert Timer Item"))

(defhydra hydra-org-change-date (:color blue :columns 4)
  "Change Date"
  ("l" org-shiftright "1 Day Later")
  ("h" org-shiftleft "1 Day Before")
  ("k" org-shiftup "1 ... Later")
  ("j" org-shiftdown "1 ... Before"))

(defhydra hydra-org-mode (:color blue :columns 4)
  "Org"
  ("b" hydra-org-babel/body "Babel")
  ("t" hydra-org-time/body "Time and Scheduling")
  ("h" hydra-org-hyperlink/body "Hyperlinks"))

(+add-mode-command #'hydra-org-mode/body '(org-mode))

(provide 'hydra-org)
;;; hydra-org.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
