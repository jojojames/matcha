;; matcha-fzf-async.el --- Integration with Transient. -*- lexical-binding: t -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/matcha
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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
(require 'fzf-async nil t)

(transient-define-prefix matcha-fzf-async ()
  "fzf-async"
  [["Find Files"
    ("f" "Find" fzf-async-find)
    ("d" "Fd" fzf-async-fd)
    ("r" "Rg Files" fzf-async-rg-files)
    ("a" "Ag Files" fzf-async-ag-files)
    ("h" "Hg Files" fzf-async-hg-files)]
   ["Git"
    ("g" "Git Ls-Files" fzf-async-git-ls-files)
    ("G" "Git Grep" fzf-async-git-grep)]
   ["Grep"
    ("F" "Grep Current File" fzf-async-grep-current-file)
    ("p" "Grep" fzf-async-grep)
    ("R" "Rg" fzf-async-rg)
    ("A" "Ag" fzf-async-ag)
    ("u" "Ugrep" fzf-async-ugrep)]
   ["Search"
    ("l" "Locate" fzf-async-locate)
    ("s" "Spotlight" fzf-async-spotlight)
    ("S" "Spotlight Apps" fzf-async-spotlight-apps)
    ("t" "Tramp" fzf-async-tramp)]
   ["Swiper"
    ("w" "Swiper" fzf-async-swiper)
    ("W" "Swiper All" fzf-async-swiper-all)]])

(provide 'matcha-fzf-async)
;;; matcha-fzf-async.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
