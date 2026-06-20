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

;; Dispatch helpers — used by sub-prefix suffix commands to honor the
;; active sub-prefix's --dir= / --backend= infix arguments.

(defun matcha-fzfa--with-dir (dir-spec fn)
  "Invoke FN with `fzfa-directory' bound per DIR-SPEC.
DIR-SPEC is \"current\", \"project\", or anything else (= no override)."
  (pcase dir-spec
    ("current"
     (let ((fzfa-directory default-directory))
       (funcall fn)))
    ("project"
     (let ((fzfa-directory (when-let* ((pr (project-current)))
                             (project-root pr))))
       (funcall fn)))
    (_ (funcall fn))))

(defun matcha-fzfa--invoke (base prefix)
  "Invoke BASE under PREFIX's --dir= infix env."
  (let* ((args (transient-args prefix))
         (dir  (transient-arg-value "--dir=" args)))
    (matcha-fzfa--with-dir dir (lambda () (call-interactively base)))))

(defun matcha-fzfa--vcs-cmd (op args)
  "Return the fzfa command implementing VCS OP for the --backend= in ARGS.
OP is a string like \"modified-locally\".  Backend defaults to \"smart\",
which dispatches via `fzfa-vc-*' (uses `vc-responsible-backend')."
  (let* ((backend (or (transient-arg-value "--backend=" args) "smart"))
         (sym
          (pcase (cons op backend)
            (`("ls-files" . "hg")    'fzfa-hg-files)
            (`("ls-files" . "smart") 'fzfa-vc-modified-files)
            (`(,_ . "smart")         (intern (concat "fzfa-vc-" op)))
            (`(,_ . ,b)              (intern (format "fzfa-%s-%s" b op))))))
    (unless (fboundp sym)
      (user-error "matcha-fzfa: %s has no `%s' implementation" backend op))
    sym))

(defun matcha-fzfa--vcs-invoke (op)
  "Invoke the VCS command for OP under the active --backend= / --dir= infixes."
  (let* ((args (transient-args 'matcha-fzfa-vcs--def))
         (cmd  (matcha-fzfa--vcs-cmd op args))
         (dir  (transient-arg-value "--dir=" args)))
    (matcha-fzfa--with-dir dir (lambda () (call-interactively cmd)))))

(defmacro matcha-fzfa--def-dispatch (name base prefix)
  "Define NAME as an interactive command invoking BASE under PREFIX's infix env."
  `(defun ,name ()
     (interactive)
     (matcha-fzfa--invoke ',base ',prefix)))

(defmacro matcha-fzfa--def-vcs (name op)
  "Define NAME as an interactive command invoking VCS OP under active backend."
  `(defun ,name ()
     (interactive)
     (matcha-fzfa--vcs-invoke ,op)))

;; Find suffix dispatchers (read matcha-fzfa-find--def's infix env).
(matcha-fzfa--def-dispatch matcha-fzfa--find-smart  fzfa-smart-find  matcha-fzfa-find--def)
(matcha-fzfa--def-dispatch matcha-fzfa--find-fd     fzfa-fd          matcha-fzfa-find--def)
(matcha-fzfa--def-dispatch matcha-fzfa--find-rg     fzfa-rg-files    matcha-fzfa-find--def)
(matcha-fzfa--def-dispatch matcha-fzfa--find-ag     fzfa-ag-files    matcha-fzfa-find--def)
(matcha-fzfa--def-dispatch matcha-fzfa--find-hg     fzfa-hg-files    matcha-fzfa-find--def)
(matcha-fzfa--def-dispatch matcha-fzfa--find-find   fzfa-find        matcha-fzfa-find--def)
(matcha-fzfa--def-dispatch matcha-fzfa--find-hungry fzfa-hungry-find matcha-fzfa-find--def)

;; Grep suffix dispatchers (read matcha-fzfa-grep--def's infix env).
(matcha-fzfa--def-dispatch matcha-fzfa--grep-smart fzfa-smart-grep matcha-fzfa-grep--def)
(matcha-fzfa--def-dispatch matcha-fzfa--grep-rg    fzfa-rg         matcha-fzfa-grep--def)
(matcha-fzfa--def-dispatch matcha-fzfa--grep-ugrep fzfa-ugrep      matcha-fzfa-grep--def)
(matcha-fzfa--def-dispatch matcha-fzfa--grep-ag    fzfa-ag         matcha-fzfa-grep--def)
(matcha-fzfa--def-dispatch matcha-fzfa--grep-grep  fzfa-grep       matcha-fzfa-grep--def)
(matcha-fzfa--def-dispatch matcha-fzfa--grep-git   fzfa-git-grep   matcha-fzfa-grep--def)

;; VCS suffix dispatchers (read matcha-fzfa-vcs--def's --backend= / --dir=).
(matcha-fzfa--def-vcs matcha-fzfa--vcs-ls       "ls-files")
(matcha-fzfa--def-vcs matcha-fzfa--vcs-modified "modified-locally")
(matcha-fzfa--def-vcs matcha-fzfa--vcs-added    "added-files")
(matcha-fzfa--def-vcs matcha-fzfa--vcs-staged   "staged-for-commit")
(matcha-fzfa--def-vcs matcha-fzfa--vcs-in-head  "modified-in-head")

;;; Sub-prefix column vectors

(defvar matcha-fzfa---find-backends nil)
(defvar matcha-fzfa---find-options nil)
(defvar matcha-fzfa---grep-backends nil)
(defvar matcha-fzfa---grep-options nil)
(defvar matcha-fzfa---vcs-operations nil)
(defvar matcha-fzfa---vcs-options nil)
(defvar matcha-fzfa---project nil)
(defvar matcha-fzfa---search nil)
(defvar matcha-fzfa---shell nil)
(defvar matcha-fzfa---marks-marks nil)
(defvar matcha-fzfa---marks-evil nil)
(defvar matcha-fzfa---code-imenu nil)
(defvar matcha-fzfa---code-misc nil)
(defvar matcha-fzfa---org nil)
(defvar matcha-fzfa---mail nil)
(defvar matcha-fzfa---music nil)
(defvar matcha-fzfa---spotify nil)
(defvar matcha-fzfa---evil nil)
(defvar matcha-fzfa---chrome nil)
(defvar matcha-fzfa---firefox nil)
(defvar matcha-fzfa---safari nil)
(defvar matcha-fzfa---passwords-pass nil)
(defvar matcha-fzfa---passwords-chrome nil)
(defvar matcha-fzfa---passwords-multi nil)

;;; Top-level column vectors

(defvar matcha-fzfa---multi nil)
(defvar matcha-fzfa---find-files nil)
(defvar matcha-fzfa---grep nil)
(defvar matcha-fzfa---sources nil)
(defvar matcha-fzfa---system nil)
(defvar matcha-fzfa---emacs nil)
(defvar matcha-fzfa---swiper nil)
(defvar matcha-fzfa---code nil)
(defvar matcha-fzfa---web nil)
(defvar matcha-fzfa---flymake nil)
(defvar matcha-fzfa---apps nil)

;;; Column contents

(setq matcha-fzfa---find-backends
      ["Backends"
       ("f"   "Smart"             matcha-fzfa--find-smart)
       ("d"   "Fd"                matcha-fzfa--find-fd)
       ("r"   "Rg files"          matcha-fzfa--find-rg)
       ("a"   "Ag files"          matcha-fzfa--find-ag)
       ("h"   "Hg files"          matcha-fzfa--find-hg)
       ("F"   "Find (POSIX)"      matcha-fzfa--find-find)
       ("n"   "Hungry Find"       matcha-fzfa--find-hungry)
       ("P"   "Project Find File" fzfa-project-find-file)
       ("SPC" "Find Any"          fzfa-find-any)])

(setq matcha-fzfa---find-options
      ["Options"
       ("-d" "Directory" "--dir="
        :choices ("current" "project" "default"))])

(setq matcha-fzfa---grep-backends
      ["Backends"
       ("g" "Smart"             matcha-fzfa--grep-smart)
       ("R" "Rg"                matcha-fzfa--grep-rg)
       ("u" "Ugrep"             matcha-fzfa--grep-ugrep)
       ("A" "Ag"                matcha-fzfa--grep-ag)
       ("j" "Grep"              matcha-fzfa--grep-grep)
       ("G" "Git Grep"          matcha-fzfa--grep-git)
       ("F" "Grep Current File" fzfa-grep-current-file)])

(setq matcha-fzfa---grep-options
      ["Options"
       ("-d" "Directory" "--dir="
        :choices ("current" "project" "default"))])

(setq matcha-fzfa---vcs-operations
      ["Operations"
       ("l" "Ls / files"       matcha-fzfa--vcs-ls)
       ("m" "Modified locally" matcha-fzfa--vcs-modified)
       ("a" "Added"            matcha-fzfa--vcs-added)
       ("s" "Staged"           matcha-fzfa--vcs-staged)
       ("h" "In HEAD"          matcha-fzfa--vcs-in-head)
       ("G" "Git Grep"         fzfa-git-grep)
       ("L" "Git Log Grep"     fzfa-git-log-grep)
       ("v" "VCS Any"          fzfa-vc-any)])

(setq matcha-fzfa---vcs-options
      ["Options"
       ("-b" "Backend"   "--backend="
        :choices ("smart" "git" "hg"))
       ("-d" "Directory" "--dir="
        :choices ("current" "project" "default"))])

(setq matcha-fzfa---project
      ["Project"
       ("f" "Find File"      fzfa-project-find-file)
       ("d" "Find Dir"       fzfa-project-find-dir)
       ("b" "Buffer"         fzfa-project-buffer)
       ("e" "Recent File"    fzfa-project-recentf)
       ("p" "Switch Project" fzfa-project-switch-project)])

(setq matcha-fzfa---search
      ["Search"
       ("l" "Locate"          fzfa-locate)
       ("s" "Spotlight"       fzfa-spotlight)
       ("S" "Spotlight Apps"  fzfa-spotlight-apps)
       ("m" "Spotlight Audio" fzfa-spotlight-audio)
       ("t" "Tramp"           fzfa-tramp)])

(setq matcha-fzfa---shell
      ["Shell / Make"
       ("c" "Shell Command"         fzfa-shell-command)
       ("C" "Project Shell Command" fzfa-shell-project-command)
       ("H" "Shell History"         fzfa-shell-history)
       ("M" "Make"                  fzfa-make)
       ("R" "Make: Reset Cache"     fzfa-make-reset-cache)])

(setq matcha-fzfa---marks-marks
      ["Marks"
       ("m" "Mark"        fzfa-mark)
       ("M" "Global Mark" fzfa-global-mark)
       ("r" "Register"    fzfa-register)])

(setq matcha-fzfa---marks-evil
      ["Evil"
       ("e" "Evil Marks"     fzfa-evil-marks)
       ("E" "Evil Registers" fzfa-evil-registers)
       ("j" "Evil Jumps"     fzfa-evil-jumps)])

(setq matcha-fzfa---code-imenu
      ["Imenu"
       ("i" "Imenu"         fzfa-imenu)
       ("I" "Imenu (All)"   fzfa-imenu-all)
       ("o" "Imenu Others"  fzfa-imenu-all-but-current)
       ("e" "Eglot Symbols" fzfa-eglot-symbols)])

(setq matcha-fzfa---code-misc
      ["Outline / Errors / Info"
       ("u" "Outline"        fzfa-outline)
       ("c" "Compile Errors" fzfa-compile-error)
       ("?" "Info at point"  fzfa-info-at-point)])

(setq matcha-fzfa---org
      ["Org"
       ("h"   "Heading"       fzfa-org-heading)
       ("H"   "Heading (All)" fzfa-org-heading-all)
       ("a"   "Agenda"        fzfa-org-agenda)
       ("t"   "Todo"          fzfa-org-todo)
       ("g"   "Tags View"     fzfa-org-tags-view)
       ("l"   "Insert Link"   fzfa-org-insert-link)
       ("SPC" "Org Any"       fzfa-org-any)])

(setq matcha-fzfa---mail
      ["Mail"
       ("m" "Mail"           fzfa-mail)
       ("r" "Refresh Mail"   fzfa-mail-refresh)
       ("n" "Notmuch"        fzfa-notmuch)
       ("N" "Notmuch (Tree)" fzfa-notmuch-tree)])

(setq matcha-fzfa---music
      ["Music"
       ("m" "Music"              fzfa-music)
       ("a" "By Artist"          fzfa-music-by-artist)
       ("g" "By Genre"           fzfa-music-by-genre)
       ("p" "Playlist"           fzfa-music-playlist)
       ("P" "Playlist (Shuffle)" fzfa-music-playlist-shuffle)
       ("r" "Refresh"            fzfa-music-refresh)])

(setq matcha-fzfa---spotify
      ["Spotify"
       ("a" "Album"        fzfa-spotify-album)
       ("A" "Artist"       fzfa-spotify-artist)
       ("t" "Track"        fzfa-spotify-track)
       ("p" "Playlist"     fzfa-spotify-playlist)
       ("P" "My Playlists" fzfa-spotify-my-playlists)])

(setq matcha-fzfa---evil
      ["Evil"
       ("m"   "Marks"          fzfa-evil-marks)
       ("r"   "Registers"      fzfa-evil-registers)
       ("j"   "Jumps"          fzfa-evil-jumps)
       (":"   "Ex History"     fzfa-evil-ex-history)
       ("/"   "Search History" fzfa-evil-search-history)
       ("q"   "Command Window" fzfa-evil-command-window)
       ("SPC" "Evil Any"       fzfa-evil-any)])

(setq matcha-fzfa---chrome
      ["Chrome"
       ("b" "Bookmarks"          fzfa-chrome-bookmarks)
       ("e" "Edit Bookmark"      fzfa-chrome-edit)
       ("c" "Copy Bookmark URL"  fzfa-chrome-bookmark-copy-url)
       ("h" "History"            fzfa-chrome-history)
       ("H" "Copy History URL"   fzfa-chrome-history-copy-url)
       ("R" "Refresh"            fzfa-chrome-refresh)])

(setq matcha-fzfa---firefox
      ["Firefox"
       ("b" "Bookmarks"          fzfa-firefox-bookmarks)
       ("c" "Copy Bookmark URL"  fzfa-firefox-bookmark-copy-url)
       ("h" "History"            fzfa-firefox-history)
       ("H" "Copy History URL"   fzfa-firefox-history-copy-url)
       ("R" "Refresh"            fzfa-firefox-refresh)])

(setq matcha-fzfa---safari
      ["Safari"
       ("b" "Bookmarks"          fzfa-safari-bookmarks)
       ("c" "Copy Bookmark URL"  fzfa-safari-bookmark-copy-url)
       ("h" "History"            fzfa-safari-history)
       ("H" "Copy History URL"   fzfa-safari-history-copy-url)
       ("R" "Refresh"            fzfa-safari-refresh)])

(setq matcha-fzfa---passwords-pass
      ["Pass"
       ("c" "Copy"     fzfa-pass-copy)
       ("e" "Edit"     fzfa-pass-edit)
       ("a" "Add"      fzfa-pass-add)
       ("g" "Generate" fzfa-pass-generate)
       ("u" "URL"      fzfa-pass-url)
       ("r" "Rename"   fzfa-pass-rename)
       ("D" "Delete"   fzfa-pass-delete)])

(setq matcha-fzfa---passwords-chrome
      ["Chrome Pass"
       ("C" "Copy"     fzfa-chrome-pass-copy)
       ("U" "Username" fzfa-chrome-pass-copy-username)
       ("l" "URL"      fzfa-chrome-pass-url)])

(setq matcha-fzfa---passwords-multi
      ["Multi"
       ("M-p" "All Passwords" fzfa-passwords)])

;;; Top-level columns

(setq matcha-fzfa---multi
      ["Multi"
       ("SPC" "Find Any"      fzfa-find-any)
       ("/"   "Find Some"     fzfa-find-some)
       ("v"   "VCS Any"       fzfa-vc-any)
       ("e"   "Evil Any"      fzfa-evil-any)
       ("E"   "Evil »"        matcha-fzfa-evil)
       ("M-p" "All Passwords" fzfa-passwords)])

(setq matcha-fzfa---find-files
      ["Find Files"
       ("f" "Find (smart)" fzfa-smart-find)
       ("F" "Find »"      matcha-fzfa-find)
       ("b" "Buffer"       fzfa-buffer)
       ("r" "Recent File"  fzfa-recent-file)])

(setq matcha-fzfa---grep
      ["Grep"
       ("g" "Grep (smart)" fzfa-smart-grep)
       ("G" "Grep »"      matcha-fzfa-grep)])

(setq matcha-fzfa---sources
      ["Sources"
       ("p" "Project »" matcha-fzfa-project)
       ("V" "VCS »"     matcha-fzfa-vcs)])

(setq matcha-fzfa---system
      ["System"
       ("!" "Shell/Make »" matcha-fzfa-shell)
       ("L" "Locate/etc »" matcha-fzfa-search)])

(setq matcha-fzfa---emacs
      ["Emacs"
       ("B" "Bookmark"   fzfa-bookmark)
       ("y" "Yank Pop"   fzfa-yank-pop)
       ("T" "Theme"      fzfa-theme)
       ("x" "M-x"        fzfa-M-x)
       ("X" "M-x (mode)" fzfa-M-x-for-buffer)])

(setq matcha-fzfa---swiper
      ["Swiper"
       ("w" "Swiper"        fzfa-swiper)
       ("W" "Swiper All"    fzfa-swiper-all)
       ("n" "Hungry Swiper" fzfa-hungry-swiper)
       ("R" "Regexp"        fzfa-regexp)])

(setq matcha-fzfa---code
      ["Code"
       ("i" "Imenu/Outline »" matcha-fzfa-code)
       ("m" "Marks/Reg »"     matcha-fzfa-marks)])

(setq matcha-fzfa---web
      ["Web"
       ("C"   "Chrome »"    matcha-fzfa-chrome)
       ("M-f" "Firefox »"   matcha-fzfa-firefox)
       ("M-s" "Safari »"    matcha-fzfa-safari)
       ("P"   "Passwords »" matcha-fzfa-passwords)])

(setq matcha-fzfa---flymake
      ["Flymake / Info"
       ("z" "Flymake"         fzfa-flymake)
       ("Z" "Flymake Project" fzfa-flymake-project)
       ("?" "Info at Point"   fzfa-info-at-point)])

(setq matcha-fzfa---apps
      ["Apps"
       ("o" "Org »"     matcha-fzfa-org)
       ("M" "Mail »"    matcha-fzfa-mail)
       ("N" "Music »"   matcha-fzfa-music)
       ("S" "Spotify »" matcha-fzfa-spotify)])

;;; Sub-prefix entry points (eval'd on demand)

(defun matcha-fzfa-find ()
  (interactive)
  (eval
   `(transient-define-prefix matcha-fzfa-find--def ()
      "Fuzzy find files."
      [,matcha-fzfa---find-backends
       ,matcha-fzfa---find-options]))
  (call-interactively #'matcha-fzfa-find--def))

(defun matcha-fzfa-grep ()
  (interactive)
  (eval
   `(transient-define-prefix matcha-fzfa-grep--def ()
      "Fuzzy content search."
      [,matcha-fzfa---grep-backends
       ,matcha-fzfa---grep-options]))
  (call-interactively #'matcha-fzfa-grep--def))

(defun matcha-fzfa-vcs ()
  (interactive)
  (eval
   `(transient-define-prefix matcha-fzfa-vcs--def ()
      "VCS operations.  Backend defaults to smart auto-dispatch."
      [,matcha-fzfa---vcs-operations
       ,matcha-fzfa---vcs-options]))
  (call-interactively #'matcha-fzfa-vcs--def))

(defun matcha-fzfa-project ()
  (interactive)
  (eval
   `(transient-define-prefix matcha-fzfa-project--def ()
      "Project commands."
      [,matcha-fzfa---project]))
  (call-interactively #'matcha-fzfa-project--def))

(defun matcha-fzfa-search ()
  (interactive)
  (eval
   `(transient-define-prefix matcha-fzfa-search--def ()
      "System search."
      [,matcha-fzfa---search]))
  (call-interactively #'matcha-fzfa-search--def))

(defun matcha-fzfa-shell ()
  (interactive)
  (eval
   `(transient-define-prefix matcha-fzfa-shell--def ()
      "Shell / Make."
      [,matcha-fzfa---shell]))
  (call-interactively #'matcha-fzfa-shell--def))

(defun matcha-fzfa-marks ()
  (interactive)
  (eval
   `(transient-define-prefix matcha-fzfa-marks--def ()
      "Marks & Registers."
      [,matcha-fzfa---marks-marks
       ,matcha-fzfa---marks-evil]))
  (call-interactively #'matcha-fzfa-marks--def))

(defun matcha-fzfa-code ()
  (interactive)
  (eval
   `(transient-define-prefix matcha-fzfa-code--def ()
      "Code navigation."
      [,matcha-fzfa---code-imenu
       ,matcha-fzfa---code-misc]))
  (call-interactively #'matcha-fzfa-code--def))

(defun matcha-fzfa-org ()
  (interactive)
  (eval
   `(transient-define-prefix matcha-fzfa-org--def ()
      "Org commands."
      [,matcha-fzfa---org]))
  (call-interactively #'matcha-fzfa-org--def))

(defun matcha-fzfa-mail ()
  (interactive)
  (eval
   `(transient-define-prefix matcha-fzfa-mail--def ()
      "Mail."
      [,matcha-fzfa---mail]))
  (call-interactively #'matcha-fzfa-mail--def))

(defun matcha-fzfa-music ()
  (interactive)
  (eval
   `(transient-define-prefix matcha-fzfa-music--def ()
      "Apple Music."
      [,matcha-fzfa---music]))
  (call-interactively #'matcha-fzfa-music--def))

(defun matcha-fzfa-spotify ()
  (interactive)
  (eval
   `(transient-define-prefix matcha-fzfa-spotify--def ()
      "Spotify."
      [,matcha-fzfa---spotify]))
  (call-interactively #'matcha-fzfa-spotify--def))

(defun matcha-fzfa-evil ()
  (interactive)
  (eval
   `(transient-define-prefix matcha-fzfa-evil--def ()
      "Evil."
      [,matcha-fzfa---evil]))
  (call-interactively #'matcha-fzfa-evil--def))

(defun matcha-fzfa-chrome ()
  (interactive)
  (eval
   `(transient-define-prefix matcha-fzfa-chrome--def ()
      "Chrome."
      [,matcha-fzfa---chrome]))
  (call-interactively #'matcha-fzfa-chrome--def))

(defun matcha-fzfa-firefox ()
  (interactive)
  (eval
   `(transient-define-prefix matcha-fzfa-firefox--def ()
      "Firefox."
      [,matcha-fzfa---firefox]))
  (call-interactively #'matcha-fzfa-firefox--def))

(defun matcha-fzfa-safari ()
  (interactive)
  (eval
   `(transient-define-prefix matcha-fzfa-safari--def ()
      "Safari."
      [,matcha-fzfa---safari]))
  (call-interactively #'matcha-fzfa-safari--def))

(defun matcha-fzfa-passwords ()
  (interactive)
  (eval
   `(transient-define-prefix matcha-fzfa-passwords--def ()
      "Passwords."
      [,matcha-fzfa---passwords-pass
       ,matcha-fzfa---passwords-chrome
       ,matcha-fzfa---passwords-multi]))
  (call-interactively #'matcha-fzfa-passwords--def))

;;; Top-level entry points

(defun matcha-fzfa--wide ()
  (eval
   `(transient-define-prefix matcha-fzfa--wide--def ()
      "fzfa"
      [,matcha-fzfa---multi
       ,matcha-fzfa---find-files
       ,matcha-fzfa---grep
       ,matcha-fzfa---sources
       ,matcha-fzfa---system
       ,matcha-fzfa---emacs]
      [,matcha-fzfa---swiper
       ,matcha-fzfa---code
       ,matcha-fzfa---web
       ,matcha-fzfa---flymake
       ,matcha-fzfa---apps]))
  (call-interactively #'matcha-fzfa--wide--def))

(defun matcha-fzfa--narrow ()
  (eval
   `(transient-define-prefix matcha-fzfa--narrow--def ()
      "fzfa"
      [,matcha-fzfa---multi
       ,matcha-fzfa---find-files
       ,matcha-fzfa---grep
       ,matcha-fzfa---sources]
      [,matcha-fzfa---system
       ,matcha-fzfa---emacs
       ,matcha-fzfa---swiper
       ,matcha-fzfa---code]
      [,matcha-fzfa---web
       ,matcha-fzfa---flymake
       ,matcha-fzfa---apps]))
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
