;; matcha-project.el --- Integration with Transient. -*- lexical-binding: t -*-

;; Copyright (C) 2021 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/matcha
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
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
;; (require 'project)

;;;###autoload
(defun project-recentf ()
  "Show a list of recently visited files in a project."
  (interactive)
  (if (boundp 'recentf-list)
      (let* ((recent-project-files (project-recentf-files))
             (completion-ignore-case read-file-name-completion-ignore-case)
             (file (funcall project-read-file-name-function
                            "Find recent file" recent-project-files nil nil
                            (thing-at-point 'filename))))
        (if (string= file "")
            (user-error "You didn't specify the file")
          (find-file file)))
    (message "recentf is not enabled")))

(defun project-recentf-files ()
  "Return a list of recently visited files in a project."
  (and (boundp 'recentf-list)
       (let* ((pr (project-current t))
              (project-root (expand-file-name (project-root pr))))
         (cl-remove-if-not
          (lambda (f)
            (string-prefix-p project-root (expand-file-name f)))
          recentf-list))))

;;;###autoload
(defun project-multi-occur (&optional nlines)
  "Do a `multi-occur' in the project's buffers.
With a prefix argument, show NLINES of context."
  (interactive "P")
  (let ((pr (project-current t)))
    (multi-occur (project--buffer-list pr)
                 (car (occur-read-primary-args))
                 nlines)))

(define-transient-command matcha-project ()
  "Project"
  [["Find"
    ("f" "File" project-find-file)
    ("F" "File or External" project-or-external-find-file)
    ("r" "Recent File" project-recentf)]
   ["Buffers"
    ("b" "Buffer" project-switch-to-buffer)
    ("K" "Kill Project Buffers" project-kill-buffers)]
   ["Actions"
    ("R" "Replace Regexp" project-query-replace-regexp)
    ("m" "Compile Project" project-compile)
    ("c" "Shell Command &" project-async-shell-command)
    ("C" "Shell Command" project-shell-command)]]
  [["Modes"
    ("g" "Version Control" project-vc-dir)
    ("h" "Dired" project-dired)
    ("e" "Eshell" project-eshell)
    ("y" "Shell" project-shell)]
   ["Search"
    ("a" "Find REGEXP" project-find-regexp)
    ("A" "Find REGEXP or External" project-or-external-find-regexp)
    ("s" "Multi Occur" project-multi-occur)]
   ["Manage"
    ("p" "Switch Project" project-switch-project)]])

(provide 'matcha-project)
;;; matcha-project.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
