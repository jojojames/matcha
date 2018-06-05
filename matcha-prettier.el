(require 'matcha-base)
;; (require 'prettier-js)

(defun matcha-prettier-or-indent-region-or-buffer ()
  "Format with `prettier-js' or use `matcha-indent-region-or-buffer'."
  (interactive)
  (cond
   ((or (null buffer-file-name)
        (string-equal (file-name-extension buffer-file-name) "html"))
    (matcha-indent-region-or-buffer))
   ((and (eq major-mode 'web-mode)
         buffer-file-name
         (or (string-match "\\.jsx?\\'" buffer-file-name)
             (string-match "\\.tsx?\\'" buffer-file-name))
         (executable-find "prettier")
         (fboundp 'prettier-js))
    (prettier-js))
   (:default
    (matcha-indent-region-or-buffer))))

(provide 'matcha-prettier)
