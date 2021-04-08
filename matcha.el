;;; matcha.el --- Integration with Hydra. -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/matcha
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (transient "20200601"))
;; Keywords: emacs, transient
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

;; Compatibility
(eval-and-compile
  (with-no-warnings
    (if (version< emacs-version "26")
        (progn
          (defalias 'matcha-if-let* #'if-let)
          (defalias 'matcha-when-let* #'when-let)
          (function-put #'matcha-if-let* 'lisp-indent-function 2)
          (function-put #'matcha-when-let* 'lisp-indent-function 1))
      (defalias 'matcha-if-let* #'if-let*)
      (defalias 'matcha-when-let* #'when-let*))))

(defgroup matcha nil
  "Collection of hydras and a common way to launch them."
  :group 'tools
  :group 'convenience)

(defcustom matcha-use-launcher-p t
  "Whether or not to use hydra launcher."
  :type 'bool
  :group 'matcha)

(defcustom matcha-me-p t
  "Whether to load personal `matcha'."
  :type 'bool
  :group 'matcha)

(defcustom matcha-use-evil-p t
  "Whether to load custom `evil' bindings for `matcha'."
  :type 'bool
  :group 'matcha)

(defcustom matcha-mode-list
  '(alchemist
    android-mode
    cc-mode ;; `java-mode'
    dired
    eglot
    go-mode
    gud-lldb
    (js js2-mode rjsx-mode)
    json-mode
    log-edit
    (:file magit :autoloads matcha-magit)
    omnisharp
    org
    pass
    (:file project :autoloads matcha-project)
    (:file projectile :autoloads matcha-projectile)
    python
    restclient
    rust-mode
    rtags
    slime
    smerge-mode
    swift-mode
    tide
    term
    vc-dir
    vc-git
    vc-hgcmd
    web-mode)
  "The list of modes for which a hydra will be defined."
  :type '(repeat (choice symbol sexp))
  :group 'matcha)

;;;###autoload
(defun matcha-setup ()
  "Set up hydras."
  (interactive)
  (when matcha-me-p
    (require 'matcha-me))
  ;; Require `elisp' by default.
  ;; It contains a lot of default functions.
  (require 'matcha-elisp)
  (dolist (entry matcha-mode-list)
    (pcase entry
      ;; (:file a :autoloads b)
      ;; (:file a :autoloads (a b c))
      (`(:file ,file :autoloads ,autoloads)
       (let ((list-autoloads (if (consp autoloads)
                                 autoloads
                               (list autoloads))))
         (matcha-setup-autoloads file list-autoloads)))
      ;; '(:modes a :autoloads b)
      ;; '(:modes (a b c) :autoloads (d e f))
      (`(:modes ,modes :autoloads ,autoloads)
       (let ((list-modes (if (consp modes)
                             modes
                           (list modes)))
             (list-autoloads (if (consp autoloads)
                                 autoloads
                               (list autoloads))))
         (matcha-require-and-setup (car list-modes) list-modes list-autoloads)))
      ;; '(a b)
      ;; '(a b c)
      (`(,mode . ,_)
       (matcha-require-and-setup mode entry))
      ;; 'a
      (`,mode
       (matcha-require-and-setup mode (list mode))))))

(defun matcha-require-and-setup (mode requires &optional autoloads)
  "Bootstrap `matcha' `hydra' using MODE as key.

MODE is used to derived where the file/require exists as well as the setup
function to call.

REQUIRES is the list of packages that will trigger setup to be called.

AUTOLOADS is a list of functions that can be autoloaded from MODE-file."
  (let ((derived-mode-sym (intern (format "matcha-%S" mode)))
        (setup-function (intern (format "matcha-%S-set-launcher" mode))))
    (when autoloads
      (mapc (lambda (auto)
              (autoload auto (symbol-name mode) nil t))
            autoloads))
    (mapc (lambda (req)
            (with-eval-after-load req
              (require derived-mode-sym)
              (when matcha-use-launcher-p
                (when (fboundp setup-function)
                  (funcall setup-function)))))
          requires)))

(defun matcha-setup-autoloads (file autoloads)
  "Bootstrap `matcha' with FILE AUTOLOADS.

This doesn't call the FILE's setup function it it has one.
Use `matcha-require-and-setup' in that case."
  (let ((load-file (format "matcha-%S" file)))
    (dolist (autoload autoloads)
      (autoload autoload load-file nil t))))

(provide 'matcha)
;;; matcha.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
