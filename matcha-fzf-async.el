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

(defvar fzf-async-directory)

(defun matcha-fzf-async-rg-current ()
  "Call `fzf-async-rg' with current directory."
  (interactive)
  (let ((fzf-async-directory default-directory))
    (fzf-async-rg)))

(defvar matcha-fzf-async---find-files nil)
(defvar matcha-fzf-async---git nil)
(defvar matcha-fzf-async---grep nil)
(defvar matcha-fzf-async---shell nil)
(defvar matcha-fzf-async---emacs nil)
(defvar matcha-fzf-async---search nil)
(defvar matcha-fzf-async---swiper nil)
(defvar matcha-fzf-async---hungry nil)

(setq matcha-fzf-async---find-files
      ["Find Files"
       ("f" "Find" fzf-async-find)
       ("d" "Fd" fzf-async-fd)
       ("r" "Rg Files" fzf-async-rg-files)
       ("a" "Ag Files" fzf-async-ag-files)
       ("h" "Hg Files" fzf-async-hg-files)])

(setq matcha-fzf-async---git
      ["Git"
       ("g" "Git Ls-Files" fzf-async-git-ls-files)
       ("G" "Git Grep" fzf-async-git-grep)])

(setq matcha-fzf-async---grep
      ["Grep"
       ("F" "Grep Current File" fzf-async-grep-current-file)
       ("p" "Grep" fzf-async-grep)
       ("R" "Rg" fzf-async-rg)
       ("A" "Ag" fzf-async-ag)
       ("u" "Ugrep" fzf-async-ugrep)])

(setq matcha-fzf-async---shell
      ["Shell"
       ("c" "Shell Command" fzf-async-shell-command)
       ("C" "Project Shell Command" fzf-async-project-shell-command)])

(setq matcha-fzf-async---emacs
      ["Emacs"
       ("b" "Buffer" fzf-async-buffer)
       ("B" "Bookmark" fzf-async-bookmark)
       ("e" "Recent File" fzf-async-recent-file)])

(setq matcha-fzf-async---search
      ["Search"
       ("l" "Locate" fzf-async-locate)
       ("s" "Spotlight" fzf-async-spotlight)
       ("S" "Spotlight Apps" fzf-async-spotlight-apps)
       ("m" "Spotlight Audio" fzf-async-spotlight-audio)
       ("t" "Tramp" fzf-async-tramp)])

(setq matcha-fzf-async---swiper
      ["Swiper"
       ("w" "Swiper" fzf-async-swiper)
       ("W" "Swiper All" fzf-async-swiper-all)])

(setq matcha-fzf-async---hungry
      ["Hungry"
       ("n" "Find Hungry" fzf-async-find-hungry)
       ("N" "Swiper Hungry" fzf-async-swiper-hungry)])

(defun matcha-fzf-async--wide ()
  (eval
   `(transient-define-prefix matcha-fzf-async--wide--def ()
      "fzf-async"
      [,matcha-fzf-async---find-files
       ,matcha-fzf-async---git
       ,matcha-fzf-async---grep
       ,matcha-fzf-async---shell
       ,matcha-fzf-async---emacs
       ,matcha-fzf-async---search
       ,matcha-fzf-async---swiper
       ,matcha-fzf-async---hungry]))
  (call-interactively #'matcha-fzf-async--wide--def))

(defun matcha-fzf-async--narrow ()
  (eval
   `(transient-define-prefix matcha-fzf-async--narrow--def ()
      "fzf-async"
      [,matcha-fzf-async---find-files
       ,matcha-fzf-async---grep
       ,matcha-fzf-async---emacs
       ,matcha-fzf-async---search]
      [,matcha-fzf-async---git
       ,matcha-fzf-async---shell
       ,matcha-fzf-async---swiper
       ,matcha-fzf-async---hungry]))
  (call-interactively #'matcha-fzf-async--narrow--def))

(defun matcha-fzf-async ()
  (interactive)
  (if (> (/ (float (frame-pixel-width)) (display-pixel-width)) 0.5)
      (matcha-fzf-async--wide)
    (matcha-fzf-async--narrow)))

(provide 'matcha-fzf-async)
;;; matcha-fzf-async.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
