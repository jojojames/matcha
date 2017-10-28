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

(with-eval-after-load 'js-mode
  (require 'hydra-javascript))

(with-eval-after-load 'js2-mode
  (require 'hydra-javascript))

(autoload 'hydra-magit/body "hydra-magit.el" nil t)
(with-eval-after-load 'magit
  (require 'hydra-magit))

(autoload 'hydra-p4/body "hydra-p4.el" nil t)
(with-eval-after-load 'p4
  (require 'hydra-p4))

(with-eval-after-load 'rjsx-mode
  (require 'hydra-javascript))

(with-eval-after-load 'smerge-mode
  (require 'hydra-smerge))

(with-eval-after-load 'typescript-mode
  (require 'hydra-typescript))

(provide 'hydra-integrations)
;;; hydra-integrations.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
