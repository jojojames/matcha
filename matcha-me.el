;;; matcha.el --- Integration with Hydra. -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Nguyen

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
;;;
;;;  USAGE:
;;;
;;;  (general-define-key
;;;   :states '(normal visual motion)
;;;   :keymaps 'override
;;;   "SPC" 'matcha-me-space)
;;;

;;; Code:
(require 'matcha-base)

(defun matcha-me-find-init ()
  "Visit init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun matcha-me-vc-dir (dir &optional backend)
  "Reimplementation of `vc-dir' without popping to another window."
  (interactive
   (list
    ;; When you hit C-x v d in a visited VC file,
    ;; the *vc-dir* buffer visits the directory under its truename;
    ;; therefore it makes sense to always do that.
    ;; Otherwise if you do C-x v d -> C-x C-f -> C-c v d
    ;; you may get a new *vc-dir* buffer, different from the original
    (file-truename (read-directory-name "VC status for directory: "
                                        (vc-root-dir) nil t
                                        nil))
    (if current-prefix-arg
        (intern
         (completing-read
          "Use VC backend: "
          (mapcar (lambda (b) (list (symbol-name b)))
                  vc-handled-backends)
          nil t nil nil)))))
  (unless backend
    (setq backend (vc-responsible-backend dir)))
  (switch-to-buffer
   (vc-dir-prepare-status-buffer "*vc-dir*" dir backend))
  (if (derived-mode-p 'vc-dir-mode)
      (vc-dir-refresh)
    (defvar use-vc-backend)
    (let ((use-vc-backend backend))
      (ignore use-vc-backend)
      (vc-dir-mode))))

(defun matcha-me-find-file-dwim ()
  "Find file DWIM."
  (interactive)
  (cond
   ((or (eq major-mode 'dired-mode)
        (eq major-mode 'dired-sidebar-mode))
    (let ((default-directory (dired-current-directory)))
      (call-interactively #'find-file)))
   ((derived-mode-p 'magit-mode)
    (let ((magit-file (magit-file-at-point)))
      (if magit-file
          (let ((default-directory
                  (file-name-directory
                   (concat (magit-toplevel) magit-file))))
            (call-interactively #'find-file))
        (call-interactively #'find-file))))
   (:default
    (call-interactively #'find-file))))

(defun matcha-me-recentf-dwim ()
  "Find recent files DWIM."
  (interactive)
  (cond
   ((bound-and-true-p ivy-mode)
    (counsel-recentf))
   ((bound-and-true-p helm-mode)
    (helm-recentf))
   ((bound-and-true-p ido-mode)
    (ido-recentf-open))
   (:else
    (recentf-open-files))))

(defun matcha-me-buffers-dwim ()
  "List buffers DWIM."
  (interactive)
  (cond
   ((bound-and-true-p ivy-mode)
    (ivy-switch-buffer))
   ((bound-and-true-p helm-mode)
    (helm-buffers-list))
   ((bound-and-true-p ido-mode)
    (ido-switch-buffer))
   (:else
    (call-interactively #'switch-to-buffer))))

(defun matcha-me-save-all-buffers ()
  "Save all buffers without confirming."
  (interactive)
  (save-some-buffers :all-buffers-no-confirm))

(define-transient-command matcha-org-space
  "Org"
  [["Org"
    ("a" "Agenda" org-agenda)
    ("c" "Capture" org-capture)
    ("r" "Refile" org-refile)
    ("o" "Todo" org-todo)]
   ["Links"
    ("l" "Store" org-store-link)
    ("i" "Insert" org-insert-link)]])

(define-transient-command matcha-me-space ()
  "Space"
  [["Find"
    ("f" "File" matcha-me-find-file-dwim)
    ("b" "Buffer" matcha-me-buffers-dwim)
    ("r" "Recent" matcha-me-recentf-dwim)
    ("n" "Sidebar" dired-sidebar-toggle-sidebar)
    ("SPC" "In Project" j-search)]
   ["Manage"
    ("w" "Window..." matcha-me-window)
    ("g" "Git..." matcha-magit)
    ("G" "Version Control" matcha-me-vc-dir)
    ("p" "Project..." matcha-projectile)
    ("y" "System..." matcha-me-system)]
   ["Do"
    ("s" "Search..." matcha-me-search)
    ("S" "Save all Buffers" matcha-me-save-all-buffers)
    ("R" "Refactor..." matcha-run-refactor-command)
    ("v" "Edit Config" matcha-me-find-init)
    ("o" "Org..." matcha-org-space)]
   ["Mode"
    ("m" "Mode" matcha-run-mode-command)
    ("d" "Debug" matcha-run-debug-command)
    ("e" "Eval" matcha-run-eval-command)
    ("t" "Test" matcha-run-test-command)
    ("=" "Format" matcha-run-format-command)]]
  [:hide (lambda () t)
         ("U" "Undo Tree" undo-tree-visualize)
         ("-" split-window-below)
         ("|" split-window-right)
         ("\\" split-window-right)
         ("h" evil-window-left)
         ("l" evil-window-right)
         ("k" evil-window-up)
         ("j" evil-window-down)
         ("." evil-next-buffer)
         ("," evil-prev-buffer)
         (";" counsel-M-x)
         (":" eval-expression)
         ("'" eval-expression)
         ("<backspace>" delete-window)
         ("DEL" delete-window) ;; For terminals.
         ("x" kill-buffer)]
  (interactive)
  (let ((transient-show-popup -.2))
    (transient-setup 'matcha-me-space)))

(define-transient-command matcha-me-profiler
  "Profiler"
  [["Profiler"
    ("s" "Start" profiler-start)
    ("r" "Report" profiler-report)
    ("x" "Stop" profiler-stop)]])

(define-transient-command matcha-me-bookmark
  "Bookmark"
  [["Bookmark"
    ("b" "Set" bookmark-set)
    ("d" "Delete" bookmark-delete)
    ("j" "Jump" bookmark-jump)
    ("l" "List" bookmark-bmenu-list)
    ("s" "Save" bookmark-save)]])

(define-transient-command matcha-me-system ()
  "System"
  [["System"
    ("f" "Finder" j-explorer-finder)
    ("t" "Open Terminal" j-open-terminal)
    ("i" "IRC" j-start-irc)
    ("m" "Email" j-notmuch)
    ("w" "Passwords" pass)
    ("W" "Copy Password" password-store-copy)
    ("b" "Bookmarks..." matcha-me-bookmark)]
   ["Shell"
    ("y" "Terminal" j-open-shell)
    ("e" "Eshell" eshell)]
   ["Processes"
    ("p" "Profiler..." matcha-me-profiler)
    ("L" "List Processes" list-processes)
    ("P" "Prodigy" prodigy)]])

(define-transient-command matcha-me-search ()
  "Search"
  [["Counsel"
    ("r" "Ripgrep" counsel-rg)
    ("R" "Ripgrep at Point" j-counsel-rg)
    ("s" "Swiper" swiper)
    ("S" "Swiper All" swiper-all)
    ("f" "Find File" counsel-find-file)
    ("g" "Git" counsel-git)]
   ["Occur"
    ("o" "Occur" occur)
    ("O" "Multi Occur" multi-occur)
    ("m" "Occur in Same Modes" j-multi-occur-in-this-mode)
    ("M" "Occur in Matching Buffers" j-multi-occur-in-this-mode)
    ("P" "Occur in Project" projectile-multi-occur)]
   ["Other"
    ("a" "Rgrep" rgrep)
    ("d" "Deadgrep" deadgrep)]])

(define-transient-command matcha-me-window ()
  "Window"
  [["Narrow/Widen"
    ("n" "Narrow" narrow-to-region)
    ("w" "Widen" widen)
    ("ND" "Narrow to Defun" narrow-to-defun)
    ("NP" "Narrow to Page" narrow-to-page)]
   ["Layout"
    ("." "Redo" winner-redo)
    ("," "Undo" winner-undo)]
   ["Text"
    ("+" "Increase" text-scale-increase)
    ("_" "Decrease" text-scale-decrease)]]
  [["Frame"
    ("m" "Maximize" toggle-frame-maximized)
    ("F" "Toggle Fullscreen" toggle-frame-fullscreen)
    ("0" "Delete Frame" delete-frame)
    ("1" "Delete other Frames" delete-other-frames)
    ("2" "Make Frame" make-frame-command)
    ("o" "Other Frame" other-frame)]
   ["Window"
    ("=" "Balance" balance-windows)
    ("r" "Resize Windows" j-resize-window)
    ("s" "Toggle Window Split" toggle-window-split)
    ("t" "Rotate Windows" rotate-windows)]
   ["Resize"
    ("<right>" "->" shrink-window-horizontally)
    ("<left>" "<-" enlarge-window-horizontally)
    ("<down>" "Down" shrink-window)
    ("<up>" "Up" enlarge-window)]]
  [:hide (lambda () t)
         ("-" split-window-below)
         ("|" split-window-right)
         ("\\" split-window-right)])

(provide 'matcha-me)
;;; matcha-me.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
