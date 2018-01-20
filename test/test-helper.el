;;; test-helper.el --- Helpers for matcha-test.el

(require 'ert)

;; FIXME: Adding `f' as a dependency just for this line.
(require 'f)
(let ((matcha-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path matcha-dir))
(require 'matcha)

;;; test-helper.el ends here
