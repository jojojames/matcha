;;; hydra-integrations.el --- Integration with Hydra. -*- lexical-binding: t -*-

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

(require 'hydra-elisp)

(with-eval-after-load 'alchemist
  (require 'hydra-elixir))

(with-eval-after-load 'android-mode
  (require 'hydra-android))

(with-eval-after-load 'cider
  (require 'hydra-clojure))

(with-eval-after-load 'lisp-mode
  (require 'hydra-commonlisp))

(with-eval-after-load 'erlang
  (require 'hydra-erlang))

(with-eval-after-load 'geiser
  (require 'hydra-scheme))

(with-eval-after-load 'java-mode
  (require 'hydra-java))

(with-eval-after-load 'js-mode
  (require 'hydra-javascript))

(with-eval-after-load 'js2-mode
  (require 'hydra-javascript))

(with-eval-after-load 'lua-mode
  (require 'hydra-lua))

(autoload 'hydra-magit/body "hydra-magit.el" nil t)
(with-eval-after-load 'magit
  (require 'hydra-magit))

(with-eval-after-load 'motion-mode
  (require 'hydra-motion))

(with-eval-after-load 'omnisharp
  (require 'hydra-csharp))

(autoload 'hydra-p4/body "hydra-p4.el" nil t)
(with-eval-after-load 'p4
  (require 'hydra-p4))

(with-eval-after-load 'pass
  (require 'hydra-pass))

(with-eval-after-load 'python
  (require 'hydra-python))

(with-eval-after-load 'rjsx-mode
  (require 'hydra-javascript))

(with-eval-after-load 'ruby-mode
  (require 'hydra-ruby))

(with-eval-after-load 'smerge-mode
  (require 'hydra-smerge))

(with-eval-after-load 'swift-mode
  (require 'hydra-swift))

(with-eval-after-load 'term
  (require 'hydra-term))

(with-eval-after-load 'typescript-mode
  (require 'hydra-typescript))

(provide 'hydra-integrations)
;;; hydra-integrations.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
