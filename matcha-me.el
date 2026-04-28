;;; matcha.el --- Integration with Transient. -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/matcha
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
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

(matcha-create-deferred-fn dired-sidebar-toggle-sidebar)
(matcha-create-deferred-interactive-fn toggle-window-dedicated)

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
      (call-interactively 'matcha-me-file)))
   ((derived-mode-p 'magit-mode)
    (let ((magit-file (magit-file-at-point)))
      (if magit-file
          (let ((default-directory
                 (file-name-directory
                  (concat (magit-toplevel) magit-file))))
            (call-interactively 'matcha-me-file))
        (call-interactively 'matcha-me-file))))
   (:default
    (call-interactively 'matcha-me-file))))

(defun matcha-me-consult-ripgrep-default-directory ()
  "Run `consult-ripgrep' with `default-directory'."
  (interactive)
  (consult-ripgrep default-directory))

(defun matcha-me-counsel-rg-default-directory ()
  "Run `counsel-rg' with `default-directory'."
  (interactive)
  (counsel-rg nil default-directory))

(defun matcha-me-deadgrep-default-directory ()
  "Run `deadgrep' with `default-directory'."
  (interactive)
  (deadgrep (deadgrep--read-search-term) default-directory))

(defcustom matcha-project-pkg-list
  '(
    ((mode . vertico-mode)
     (file . find-file)
     (recent . consult-recent-file)
     (buffer . switch-to-buffer)
     (rg . matcha-me-consult-ripgrep-default-directory)
     (rg-project . consult-ripgrep)
     (mx . execute-extended-command)
     (swiper . consult-line)
     (swiper-all . consult-line-multi)
     (git-grep . consult-git-grep))

    ((mode . selectrum-mode)
     (file . find-file)
     (recent . consult-recent-file)
     (buffer . switch-to-buffer)
     (rg . matcha-me-consult-ripgrep-default-directory)
     (rg-project . consult-ripgrep)
     (mx . execute-extended-command)
     (swiper . consult-line)
     (swiper-all . consult-line-multi)
     (git-grep . consult-git-grep))

    ((mode . ivy-mode)
     (file . counsel-find-file)
     (recent . counsel-recentf)
     (buffer . ivy-switch-buffer)
     (rg . matcha-me-counsel-rg-default-directory)
     (rg-project . counsel-rg)
     (mx . counsel-M-x)
     (swiper . swiper)
     (swiper-all . swiper-all)
     (git-grep . counsel-git-grep))

    ((mode . helm-mode)
     (find . helm-find-files)
     (recent . helm-recentf)
     (buffer . helm-buffers-list)
     (mx . helm-M-x)
     (swiper . helm-swoop)
     (swiper-all . helm-multi-swoop)
     (git-grep . helm-grep-do-git-grep))

    ((mode . ido-mode)
     (file . ido-find-file)
     (recent . ido-recentf-open)
     (buffer . ido-switch-buffer))

    ((fallback . t)
     (file . find-file)
     (recent . recentf-open-files)
     (buffer . switch-to-buffer)
     (mx . execute-extended-command)
     (git-grep . vc-git-grep))
    )
  "List of alists of common commands that different packages provide."
  :type '(repeat sexp)
  :group 'matcha)

(defmacro matcha-create-project-actions (&rest actions)
  "Create a function to run a project action.

ACTIONS has to be a key in `matcha-project-pkg-list'
that's not :mode or :fallback."
  `(progn
     ,@(cl-loop
        for action in actions
        appending
        (let ((last-func (intern (format "matcha-me-%S-last-used" action)))
              (func-name (intern (format "matcha-me-%S" action))))
          `((defvar ,last-func nil)
            (defun ,func-name ()
              ,(format "Run %S in editor." action)
              (interactive)
              (catch 'done
                (dolist (pkg matcha-project-pkg-list)
                  (if (alist-get 'fallback pkg)
                      (let ((fn (alist-get ',action pkg)))
                        (when fn
                          (call-interactively fn)
                          (throw 'done fn)))
                    (let ((mode (alist-get 'mode pkg))
                          (fn (alist-get ',action pkg)))
                      (when (boundp mode)
                        (when (and ,last-func
                                   (not (eq fn ,last-func)))
                          (apply mode '(1)))
                        (setq ,last-func fn)
                        (when fn
                          (call-interactively fn)
                          (throw 'done fn)))))))))))))

(matcha-create-project-actions
 file
 recent
 buffer
 rg
 rg-project
 mx
 swiper
 swiper-all
 git-grep)

(defun matcha-me-save-all-buffers ()
  "Save all buffers without confirming."
  (interactive)
  (save-some-buffers :all-buffers-no-confirm))

;; File Related

(defun matcha-copy-current-filename-to-clipboard ()
  "Copy `buffer-file-name' to system clipboard."
  (interactive)
  (if (not buffer-file-name)
      (message "Not a file...")
    (message (format "Copying %s to clipboard..." buffer-file-name))
    (kill-new buffer-file-name)))

(defun matcha-copy-current-filename-base-to-clipboard ()
  "Copy file name to system clipboard."
  (interactive)
  (if (not buffer-file-name)
      (message "Not a file...")
    (let ((file (format "%s.%s"
                        (file-name-base buffer-file-name)
                        (file-name-extension buffer-file-name))))
      (message (format "Copying %s to clipboard..." file))
      (kill-new file))))

(defalias 'copy-current-filename-to-clipboard 'matcha-copy-current-filename-to-clipboard)
(defalias 'copy-current-filename-base-to-clipboard 'matcha-copy-current-filename-base-to-clipboard)

(defun matcha-revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed.

https://emacs.stackexchange.com/questions/24459/revert-all-open-buffers-and-ignore-errors"
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (when (file-readable-p filename)
          ;; If the file exists and is readable, revert the buffer.
          (with-current-buffer buf
            (revert-buffer :ignore-auto :noconfirm :preserve-modes))))))
  (message "Finished reverting buffers containing unmodified files."))

(defalias 'revert-all-file-buffers 'matcha-revert-all-file-buffers)

(defvar matcha-saved-files-file "~/.emacs.d/saved-files")

(defun matcha-save-files-to-saved-files-list ()
  "Save list of open files in Emacs to `matcha-saved-files-file'."
  (interactive)
  (let ((text-to-write ""))
    (dolist (buffer (buffer-list))
      (when-let* ((buffer-name (buffer-file-name buffer)))
        (setq text-to-write (concat text-to-write buffer-name "\n"))))
    (unless (string-equal text-to-write "")
      (message (format "Writing to %s..." matcha-saved-files-file))
      (delete-file matcha-saved-files-file)
      (write-region text-to-write nil matcha-saved-files-file))))

(defalias 'save-files-to-saved-files-list 'matcha-save-files-to-saved-files-list)

(defun matcha-open-files-from-saved-files-list ()
  "Open saved files stored at `matcha-saved-files-file'."
  (interactive)
  (let ((files (with-temp-buffer
                 (insert-file-contents matcha-saved-files-file)
                 (split-string (buffer-string) "\n" t))))
    (message (format "Reading from %s..." matcha-saved-files-file))
    (mapc (lambda (file)
            (if (file-exists-p file)
                (ignore-errors
                  (message (format "Finding file %s..." file))
                  (find-file file))
              (message (format "File %s doesn't exist anymore." file))))
          files)
    (message "Finish opening saved files.")))

(defalias 'open-files-from-saved-files-list 'matcha-open-files-from-saved-files-list)

(defun matcha-rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defalias 'rename-current-buffer-file 'matcha-rename-current-buffer-file)

(defvar org-directory)
(defun matcha-me-org-find-file ()
  "Find file in `org-directory'."
  (interactive)
  (let ((default-directory (concat org-directory "/")))
    (call-interactively 'find-file)))

(defun matcha-open-terminal ()
  "Open system terminal."
  (interactive)
  (cond
   (MAC-P
    (shell-command
     ;; open -a Terminal doesn't allow us to open a particular directory unless
     ;; We use --args AND -n, but -n opens an entirely new Terminal application
     ;; instance on every call, not just a new window. Using the
     ;; bundle here always opens the given directory in a new window.
     (concat "open -b com.apple.terminal " default-directory) nil nil))
   (WINDOWS-P
    ;; https://stackoverflow.com/questions/13505113/how-to-open-the-native-cmd-exe-window-in-emacs
    (let ((proc (start-process "cmd" nil "cmd.exe" "/C" "start" "cmd.exe")))
      (set-process-query-on-exit-flag proc nil)))
   (t
    (message "Implement `matcha-open-terminal' for this OS!"))))

(defun matcha-explorer-finder ()
  "Opens up file explorer based on operating system."
  (interactive)
  (cond
   ((and MAC-P
         (fboundp #'reveal-in-osx-finder))
    (reveal-in-osx-finder))
   ((and WINDOWS-P
         (fboundp #'explorer))
    (explorer))
   (LINUX-P
    (if (executable-find "gio")
        (progn
          (shell-command (format "gio open %s" default-directory))
          (message (format "Opened %s in file browser!" default-directory)))
      (message "On platform Linux but executable gio not found!")))
   (t
    (message "Implement `explorer-finder' for this OS!"))))

(defun matcha-open-shell ()
  "Opens up a specific terminal depending on operating system."
  (interactive)
  (if (memq system-type '(cygwin windows-nt ms-dos))
      (eshell)
    (cond ((fboundp 'ghostel) (ghostel))
          ((fboundp 'vterm) (vterm))
          (:default (multi-term)))))

(defun matcha-resize-window ()
  "Resize window to fit contents."
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((fit-window-to-buffer-horizontally t))
      (fit-window-to-buffer))))

(defun matcha-start-agent ()
  "Start up an agent."
  (interactive)
  (cond
   ((fboundp 'agent-shell)
    (agent-shell))
   ((fboundp 'eca)
    (eca))
   (:default
    (message "No."))))

;; Transients

(defun matcha-org-sort-keep-selection ()
  "Keep current selected row during sorting."
  (interactive)
  (let ((first  (org-table-get nil 1))
        (second (org-table-get nil 2))
        (column (current-column))
        (result (org-table-sort-lines nil ?N)))
    (org-table-goto-line 1)
    (search-forward-regexp (format "%s.*%s" first second))
    (beginning-of-line)
    (forward-char column)
    result))

(transient-define-prefix matcha-org-space ()
  "Org"
  [["Org"
    ("a" "Agenda" org-agenda)
    ("c" "Capture" org-capture)
    ("r" "Refile" org-refile)
    ("t" "Todo" org-todo)]
   ["Table"
    ("o" "Sort" matcha-org-sort-keep-selection)]
   ["Links"
    ("l" "Store" org-store-link)
    ("i" "Insert" org-insert-link)]
   ["Wiki"
    ("f" "Find Note" matcha-me-org-find-file)]])

(defvar matcha-me---find nil)
(defvar matcha-me---manage nil)
(defvar matcha-me---do nil)
(defvar matcha-me---mode nil)
(defvar matcha-me---windowing nil)
(defvar matcha-me---extra nil)
(defvar matcha-me---hidden-bindings nil)

(setq matcha-me---find
      ["Find"
       ("f" "File" matcha-me-find-file-dwim)
       ("b" "Buffer" matcha-me-buffer)
       ("r" "Recent" matcha-me-recent)
       ("n" "Sidebar" matcha-dired-sidebar-toggle-sidebar)
       ("SPC" "In Project" project-or-external-find-file)])

(setq matcha-me---manage
      ["Manage"
       ("g" "Git" matcha-magit)
       ("G" "Version Control" matcha-me-vc-dir)
       ("p" "Project" matcha-project)
       ("P" "Projectile" matcha-projectile)
       ("TAB" "Tabs" matcha-tab-bar)])

(setq matcha-me---do
      ["Do"
       ("s" "Search" matcha-me-search)
       ("S" "Save all Buffers" matcha-me-save-all-buffers)
       ("v" "Edit Config" matcha-me-find-init)
       ("Y" "Flymake" matcha-me-flymake)])

(setq matcha-me---mode
      ["Mode"
       ("m" "Mode" matcha-run-mode-command)
       ("d" "Debug" matcha-run-debug-command)
       ("e" "Eval" matcha-run-eval-command)
       ("t" "Test" matcha-run-test-command)
       ("=" "Format" matcha-run-format-command)
       ("R" "Refactor" matcha-run-refactor-command)])

(setq matcha-me---windowing
      ["Windowing"
       ("w" "Window" matcha-me-window)
       ("DEL" "Delete Window" delete-window)
       ("x" "Kill Buffer" kill-buffer)])

(setq matcha-me---extra
      ["Extra"
       ("F" "Manage Files" matcha-me-files)
       ("y" "System" matcha-me-system)
       ("o" "Org" matcha-org-space)
       ("i" "Agent" matcha-start-agent)])

(setq matcha-me---hidden-bindings
      [:hide (lambda () t)
             ("/" matcha-me-swiper)
             ("-" split-window-below)
             ("|" split-window-right)
             ("\\" split-window-right)
             ("h" evil-window-left)
             ("l" evil-window-right)
             ("k" evil-window-up)
             ("j" evil-window-down)
             ("." evil-next-buffer)
             ("," evil-prev-buffer)
             (";" matcha-me-mx)
             (":" eval-expression)
             ("'" eval-expression)
             ("<tab>" matcha-tab-bar)
             ("<backspace>" delete-window)])

(defun matcha-me-space--wide ()
  (eval
   `(transient-define-prefix matcha-me-space--wide--def ()
      "Space"
      :environment (lambda (fn)
                     (let ((transient-show-menu -.2)) (funcall fn)))
      [,matcha-me---find
       ,matcha-me---manage
       ,matcha-me---do
       ,matcha-me---mode
       ,matcha-me---windowing
       ,matcha-me---extra]
      ,matcha-me---hidden-bindings))
  (call-interactively #'matcha-me-space--wide--def))

(defun matcha-me-space--narrow ()
  (eval
   `(transient-define-prefix matcha-me-space--narrow--def ()
      "Space"
      :environment (lambda (fn)
                     (let ((transient-show-menu -.2)) (funcall fn)))
      [,matcha-me---find
       ,matcha-me---manage
       ,matcha-me---do]
      [,matcha-me---mode
       ,matcha-me---windowing
       ,matcha-me---extra]
      ,matcha-me---hidden-bindings))
  (call-interactively #'matcha-me-space--narrow--def))

(defun matcha-me-space ()
  (interactive)
  (if (> (/ (float (frame-pixel-width)) (display-pixel-width)) 0.5)
      (matcha-me-space--wide)
    (matcha-me-space--narrow)))

(transient-define-prefix matcha-me-profiler ()
  "Profiler"
  [["Profiler"
    ("s" "Start" profiler-start)
    ("r" "Report" profiler-report)
    ("x" "Stop" profiler-stop)]])

(transient-define-prefix matcha-me-bookmark ()
  "Bookmark"
  [["Bookmark"
    ("b" "Set" bookmark-set)
    ("d" "Delete" bookmark-delete)
    ("j" "Jump" bookmark-jump)
    ("l" "List" bookmark-bmenu-list)
    ("s" "Save" bookmark-save)]])

(transient-define-prefix matcha-me-system ()
  "System"
  [["System"
    ("f" "Finder" matcha-explorer-finder)
    ("t" "Open Terminal" matcha-open-terminal)
    ("w" "Passwords" pass)
    ("W" "Copy Password" password-store-copy)
    ("b" "Bookmarks..." matcha-me-bookmark)]
   ["Shell"
    ("y" "Terminal" matcha-open-shell)
    ("e" "Eshell" eshell)]
   ["Processes"
    ("p" "Profiler..." matcha-me-profiler)
    ("P" "Proced" proced)
    ("L" "List Processes" list-processes)]])

(transient-define-prefix matcha-me-search ()
  "Search"
  [["Buffer"
    ("s" "Swiper" matcha-me-swiper)
    ("S" "Swiper Open Buffers" matcha-me-swiper-all)]
   ["Files"
    ("f" "Find File" matcha-me-file)
    ("i" "Git Grep" matcha-me-git-grep)]
   ["Occur"
    ("o" "Occur" occur)
    ("O" "Multi Occur" multi-occur)
    ("P" "Occur in Project" projectile-multi-occur)]
   ["Ripgrep"
    ("r" "Ripgrep in Directory" matcha-me-rg)
    ("R" "Ripgrep in Project" matcha-me-rg-project)
    ("d" "Deadgrep in Directory" matcha-me-deadgrep-default-directory)
    ("D" "Deadgrep in Project" deadgrep)]
   ["Other"
    ("a" "Rgrep" rgrep)]])

(transient-define-prefix matcha-me-window ()
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
    ("r" "Resize Windows" matcha-resize-window)
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

(transient-define-prefix matcha-me-files ()
  "Files"
  [["Current File"
    ("f" "Copy Filename (Base) to Clipboard"
     matcha-copy-current-filename-base-to-clipboard)
    ("y" "Copy Filename to Clipboard" matcha-copy-current-filename-to-clipboard)
    ("r" "Rename Current File" matcha-rename-current-buffer-file)]
   ["All Files"
    ("S" "Save All to SavedFile" matcha-save-files-to-saved-files-list)
    ("O" "Open All from SavedFile" matcha-open-files-from-saved-files-list)
    ("R" "Revert/Refresh All" matcha-revert-all-file-buffers)]])

(transient-define-prefix matcha-me-flymake ()
  "Flymake"
  [["Diagnostics"
    ("l" "Go to log buffer" flymake-switch-to-log-buffer)
    ("d" "Show Diagnostic" flymake-show-diagnostic)
    ("g" "Go to Diagnostic" flymake-goto-diagnostic )
    ("b" "Show Buffer Diagnostics" flymake-show-buffer-diagnostics)
    ("p" "Show Project Diagnostics" flymake-show-project-diagnostics)]
   ["Backends"
    ("B" "Display Running Backends" flymake-running-backends)
    ("D" "Display Disabled Backends" flymake-disabled-backends)
    ("R" "Display Reporting Backends" flymake-reporting-backends)
    ]
   ["Navigate"
    ("j" "Next Error" flymake-goto-next-error)
    ("k" "Previous Error" flymake-goto-prev-error)]])

(provide 'matcha-me)
;;; matcha-me.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
