;;; matcha-base.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
(eval-when-compile (require 'subr-x))
(require 'hydra)

(defun matcha-projectile-root ()
  "Return path `matcha-projectile' can print in heading."
  (if (projectile-project-p)
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory (projectile-project-root))))
    "Not in Project"))

(defun matcha-heading-current-file ()
  "Return current file for use with `hydra' heading."
  buffer-file-name)

(defun matcha-heading-current-directory ()
  "Return path to `default-directory' for use with `hydra' heading."
  default-directory)

(defun matcha-indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (unless indent-tabs-mode
            (untabify (point-min) (point-max)))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))

(defmacro matcha-make (&rest commands)
  "Make a common interface command."
  `(progn
     ,@(cl-loop
        for command in commands
        appending
        (let* ((cmd-name (symbol-name command))
               (major-mode-fn-alist
                (intern (format "matcha-local-major-%s-fns" cmd-name)))
               (minor-mode-fn-alist
                (intern (format "matcha-local-minor-%s-fns" cmd-name))))
          `((defvar ,major-mode-fn-alist nil
              (format "Alist of major commands for %s." ,cmd-name))
            (defvar ,minor-mode-fn-alist nil
              (format "Alist of minor commands for %s." ,cmd-name))

            (cl-defun ,(intern (format "matcha-set-%s-command" cmd-name))
                (&key
                 mode
                 command
                 (minor-p nil))
              ,(format "Loop over MODE and make an alist of each mode\
pointed at COMMAND.

MODE may be a single symbol or a list of symbols.

Add it to `%S' or `%S'.

If MINOR-P is t, COMMAND will be added to `%S'."
                       minor-mode-fn-alist
                       major-mode-fn-alist
                       minor-mode-fn-alist)
              (mapc (lambda (mode)
                      (if (or (memq mode minor-mode-list) minor-p)
                          (push `(,mode . ,command) ,minor-mode-fn-alist)
                        (push `(,mode . ,command) ,major-mode-fn-alist)))
                    (if (consp mode)
                        mode
                      (list mode))))

            (defun ,(intern (format "matcha-run-%s-command" cmd-name)) ()
              ,(format "Run major or minor mode command for %s." cmd-name)
              (interactive)
              (let ((major-f (cdr (assq major-mode ,major-mode-fn-alist)))
                    (backup-f (cdr (assq 'emacs-lisp-mode ,major-mode-fn-alist))))
                (if (= (length ,minor-mode-fn-alist) 0)
                    (if major-f
                        (funcall major-f)
                      (funcall backup-f))
                  (let ((commands (if major-f
                                      (list major-f)
                                    '())))
                    (mapc (lambda (mode-f)
                            (let ((m (car mode-f))
                                  (f (cdr mode-f)))
                              (when (and (boundp m)
                                         (symbol-value m))
                                (unless (memq f commands)
                                  (push f commands)))))
                          ,minor-mode-fn-alist)
                    (cond
                     ((= (length commands) 0)
                      (funcall backup-f))
                     ((= (length commands) 1)
                      (funcall (car commands)))
                     (:default
                      (funcall
                       (matcha-normalized-title-to-matcha-command
                        (completing-read
                         "Which Hydra? "
                         (mapcar
                          (lambda (command)
                            (matcha-command-to-normalized-title command))
                          ;; We want the major mode to be first in the list.
                          (reverse commands))))))))))))))))

(defun matcha-command-to-normalized-title (matcha-command)
  "Takes in a function symbol and converts it to a human readable string.

`matcha-elisp-mode/body' -> Elisp Mode"
  (let* ((name (symbol-name matcha-command))
         (tokens (split-string name "-" t)))
    (car (split-string
          (string-trim
           (mapconcat (lambda (token)
                        (cond
                         ((string-equal token "matcha") nil)
                         (t
                          (capitalize token))))
                      tokens " "))
          "/body"))))

(defun matcha-normalized-title-to-matcha-command (title)
  "Takes in a string and returns a `hydra' function symbol.

Elisp Mode -> `matcha-elisp-mode/body'."
  (let* ((downcase (downcase title))
         (tokens (split-string downcase " "))
         (base-f (mapconcat (lambda (token)
                              token) tokens "-"))
         (f-hydra (intern (format (concat "matcha-" base-f "/body"))))
         (f-func (intern (format (concat "matcha-" base-f)))))
    ;; Handle commands added that aren't hydras.
    ;; For example, `matcha-prettier-or-indent-region-or-buffer'.
    (if (fboundp f-hydra) f-hydra f-func)))

(matcha-make debug eval format mode refactor test)

(provide 'matcha-base)
;;; matcha-base.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
