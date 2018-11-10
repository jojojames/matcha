;;; matcha-web-mode.el --- Integration with Hydra. -*- lexical-binding: t -*-

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
(require 'matcha-base)
;; (require 'web-mode)

(defhydra matcha-web-mode (:color blue :hint nil)
  "

    Web:
  ------------------------------------------------------------------------------
    _f_ Fold/Unfold              _d_ Dom
    _h_ Highlight Buffer         _b_ Block
    _i_ Indent Buffer            _a_ HTML Attribute
    _n_ Tag/Block Navigation     _e_ HTML Element
    _m_ Mark and Expand          _t_ HTML Tag
    _w_ Toggle Whitespace
    _s_ Insert Snippet

"
  ("d" matcha-web-mode-dom/body)
  ("b" matcha-web-mode-block/body)
  ("a" matcha-web-mode-html-attr/body)
  ("e" matcha-web-mode-html-element/body)
  ("t" matcha-web-mode-html-tag/body)
  ("f" web-mode-fold-or-unfold)
  ("h" web-mode-buffer-highlight)
  ("i" web-mode-buffer-indent)
  ("j" web-mode-jshint)
  ("l" web-mode-file-link)
  ("m" web-mode-mark-and-expand)
  ("n" web-mode-navigate)
  ("r" web-mode-reload)
  ("s" web-mode-snippet-insert)
  ("w" web-mode-whitespaces-show))

(defhydra matcha-web-mode-dom (:color blue :hint nil)
  "

    Dom:
  ------------------------------------------------------------------------------
    _n_ Normalise       _e_ Replace HTML Entities
    _x_ XPath           _q_ Replace Dumb Quotes
    _t_ Traverse        _a_ Replace Apostrophes
    _d_ Show Errors

"
  ("a" web-mode-dom-apostrophes-replace)
  ("d" web-mode-dom-errors-show)
  ("e" web-mode-dom-entities-replace)
  ("n" web-mode-dom-normalize)
  ("q" web-mode-dom-quotes-replace)
  ("t" web-mode-dom-traverse)
  ("x" web-mode-dom-xpath))

(defhydra matcha-web-mode-block (:color blue :hint nil)
  "

    Block:
  ------------------------------------------------------------------------------
    _s_ Select Block     _c_ Close Block      _k_ Kill Block

    _p_ Previous Block        _n_ Next Block
    _b_ Beginning of Block    _e_ End of Block

"
  ("b" web-mode-block-beginning)
  ("c" web-mode-block-close)
  ("e" web-mode-block-end)
  ("k" web-mode-block-kill)
  ("n" web-mode-block-next)
  ("p" web-mode-block-previous)
  ("s" web-mode-block-select))

(defhydra matcha-web-mode-html-attr (:color blue :hint nil)
  "

    HTML Attribute:
  ------------------------------------------------------------------------------
    _s_ Select Attribute          _t_ Transpose Attribute
    _i_ Insert Attribute          _k_ Kill Attribute

    _p_ Previous Attribute        _n_ Next Attribute
    _b_ Beginning of Attribute    _e_ End of Attribute

"
  ("b" web-mode-attribute-beginning)
  ("e" web-mode-attribute-end)
  ("i" web-mode-attribute-insert)
  ("n" web-mode-attribute-next)
  ("s" web-mode-attribute-select)
  ("k" web-mode-attribute-kill)
  ("p" web-mode-attribute-previous)
  ("t" web-mode-attribute-transpose))

(defhydra matcha-web-mode-html-element (:color blue :hint nil)
  "

    HTML Element:
  ------------------------------------------------------------------------------
    _s_ Select Element          _t_ Transpose Element
    _i_ Insert Element          _k_ Kill Element
    _w_ Wrap Element            _v_ Vanish Element
    _r_ Rename Element          _c_ Clone Element
    _/_ Close Element           _m_ Mute Blanks

    _p_ Previous Element        _n_ Next Element
    _b_ Beginning of Element    _e_ End of Element

    _u_ Parent Element          _d_ Child Element
    _a_ Select Content          _f_ Fold Child Element

"

  ("/" web-mode-element-close)
  ("a" web-mode-element-content-select)
  ("b" web-mode-element-beginning)
  ("c" web-mode-element-clone)
  ("d" web-mode-element-child)
  ("e" web-mode-element-end)
  ("f" web-mode-element-children-fold-or-unfold)
  ("i" web-mode-element-insert)
  ("k" web-mode-element-kill)
  ("m" web-mode-element-mute-blanks)
  ("n" web-mode-element-next)
  ("p" web-mode-element-previous)
  ("r" web-mode-element-rename)
  ("s" web-mode-element-select)
  ("t" web-mode-element-transpose)
  ("u" web-mode-element-parent)
  ("v" web-mode-element-vanish)
  ("w" web-mode-element-wrap))

(defhydra matcha-web-mode-html-tag (:color blue :hint nil)
  "

    HTML Tag:
  ------------------------------------------------------------------------------
    _s_ Select Tag     _a_ Sort Tag Attributes

    _p_ Previous Tag        _n_ Next Tag
    _b_ Beginning of Tag    _e_ End of Tag

"
  ("a" web-mode-tag-attributes-sort)
  ("b" web-mode-tag-beginning)
  ("e" web-mode-tag-end)
  ("m" web-mode-tag-match)
  ("n" web-mode-tag-next)
  ("p" web-mode-tag-previous)
  ("s" web-mode-tag-select))

(defun matcha-web-mode-set-launcher ()
  "Set up `web-mode' with `hydra'."
  (matcha-set-mode-command :mode 'web-mode :command #'matcha-web-mode/body))

(provide 'matcha-web-mode)
;;; matcha-web-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
