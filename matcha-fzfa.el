;; matcha-fzfa.el --- Integration with Transient. -*- lexical-binding: t -*-

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
(require 'fzfa nil t)

(defvar fzfa-directory)

(defun matcha-fzfa-rg-current ()
  "Call `fzfa-rg' with current directory."
  (interactive)
  (let ((fzfa-directory default-directory))
    (fzfa-rg)))

(defvar matcha-fzfa---multi nil)
(defvar matcha-fzfa---find-files nil)
(defvar matcha-fzfa---git nil)
(defvar matcha-fzfa---grep nil)
(defvar matcha-fzfa---shell nil)
(defvar matcha-fzfa---emacs nil)
(defvar matcha-fzfa---search nil)
(defvar matcha-fzfa---swiper nil)
(defvar matcha-fzfa---imenu nil)
(defvar matcha-fzfa---hungry nil)
(defvar matcha-fzfa---chrome nil)
(defvar matcha-fzfa---passwords nil)

(setq matcha-fzfa---multi
      ["Multi"
       ("SPC" "Find Any" fzfa-find-any)
       ("/" "Find Some" fzfa-find-some)])

(setq matcha-fzfa---find-files
      ["Find Files"
       ("f" "Find" fzfa-find)
       ("d" "Fd" fzfa-fd)
       ("r" "Rg Files" fzfa-rg-files)
       ("a" "Ag Files" fzfa-ag-files)
       ("h" "Hg Files" fzfa-hg-files)])

(setq matcha-fzfa---git
      ["Git"
       ("g" "Git Ls-Files" fzfa-git-ls-files)
       ("G" "Git Grep" fzfa-git-grep)])

(setq matcha-fzfa---grep
      ["Grep"
       ("F" "Grep Current File" fzfa-grep-current-file)
       ("p" "Grep" fzfa-grep)
       ("R" "Rg" fzfa-rg)
       ("A" "Ag" fzfa-ag)
       ("u" "Ugrep" fzfa-ugrep)])

(setq matcha-fzfa---shell
      ["Shell"
       ("c" "Shell Command" fzfa-shell-command)
       ("C" "Project Shell Command" fzfa-project-shell-command)
       ("H" "Shell History" fzfa-shell-history)])

(setq matcha-fzfa---emacs
      ["Emacs"
       ("b" "Buffer" fzfa-buffer)
       ("B" "Bookmark" fzfa-bookmark)
       ("e" "Recent File" fzfa-recent-file)
       ("y" "Yank Pop" fzfa-yank-pop)
       ("T" "Theme" fzfa-theme)])

(setq matcha-fzfa---search
      ["Search"
       ("l" "Locate" fzfa-locate)
       ("s" "Spotlight" fzfa-spotlight)
       ("S" "Spotlight Apps" fzfa-spotlight-apps)
       ("m" "Spotlight Audio" fzfa-spotlight-audio)
       ("t" "Tramp" fzfa-tramp)])

(setq matcha-fzfa---swiper
      ["Swiper"
       ("w" "Swiper" fzfa-swiper)
       ("W" "Swiper All" fzfa-swiper-all)])

(setq matcha-fzfa---imenu
      ["Imenu"
       ("i" "Imenu" fzfa-imenu)
       ("I" "Imenu All" fzfa-imenu-all)
       ("o" "Imenu Others" fzfa-imenu-all-but-current)])

(setq matcha-fzfa---hungry
      ["Hungry"
       ("n" "Find Hungry" fzfa-find-hungry)
       ("N" "Swiper Hungry" fzfa-swiper-hungry)])

(setq matcha-fzfa---chrome
      ["Chrome"
       ("k" "Bookmark" fzfa-chrome-bookmarks)
       ("K" "Edit Bookmark" fzfa-chrome-edit)])

(setq matcha-fzfa---passwords
      ["Passwords"
       ("p" "Pass" fzfa-pass)
       ("P" "Chrome Pass" fzfa-chrome-pass)
       ("M-p" "All Passwords" fzfa-passwords)])

(defun matcha-fzfa--wide ()
  (eval
   `(transient-define-prefix matcha-fzfa--wide--def ()
      "fzfa"
      [,matcha-fzfa---multi
       ,matcha-fzfa---find-files
       ,matcha-fzfa---git
       ,matcha-fzfa---grep
       ,matcha-fzfa---shell
       ,matcha-fzfa---emacs]
      [,matcha-fzfa---search
       ,matcha-fzfa---swiper
       ,matcha-fzfa---imenu
       ,matcha-fzfa---hungry
       ,matcha-fzfa---chrome
       ,matcha-fzfa---passwords]))
  (call-interactively #'matcha-fzfa--wide--def))

(defun matcha-fzfa--narrow ()
  (eval
   `(transient-define-prefix matcha-fzfa--narrow--def ()
      "fzfa"
      [,matcha-fzfa---multi
       ,matcha-fzfa---find-files
       ,matcha-fzfa---grep
       ,matcha-fzfa---emacs]
      [,matcha-fzfa---search
       ,matcha-fzfa---passwords
       ,matcha-fzfa---git
       ,matcha-fzfa---shell]
      [,matcha-fzfa---swiper
       ,matcha-fzfa---imenu
       ,matcha-fzfa---hungry
       ,matcha-fzfa---chrome]))
  (call-interactively #'matcha-fzfa--narrow--def))

(defun matcha-fzfa ()
  (interactive)
  (if (> (/ (float (frame-pixel-width)) (display-pixel-width)) 0.5)
      (matcha-fzfa--wide)
    (matcha-fzfa--narrow)))

(provide 'matcha-fzfa)
;;; matcha-fzfa.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
