;;; matcha-test.el --- Tests for matcha -*- lexical-binding: t -*-

;; http://endlessparentheses.com/understanding-letf-and-how-it-replaces-flet.html
(require 'matcha)
(require 'ert)

(ert-deftest matcha-blank-test ()
  "Blank test."
  (should (equal 0 0)))

(ert-deftest matcha-normalized-title-to-matcha-command-test ()
  "Test `matcha-normalized-title-to-matcha-command'."
  (cl-letf (((symbol-function 'matcha-elisp-mode/body) #'ignore))
    (should (eq (matcha-normalized-title-to-matcha-command "Elisp Mode")
                'matcha-elisp-mode/body)))

  ;; If function is not bound, it symbol will be returned without /body.
  (should (eq (matcha-normalized-title-to-matcha-command "Elisp Mode")
              'matcha-elisp-mode)))

(ert-deftest matcha-command-to-normalized-title-test ()
  "Test `matcha-command-to-normalized-title'."
  (should (string-equal
           (matcha-command-to-normalized-title 'matcha-elisp-mode/body)
           "Elisp Mode"))
  (should (string-equal
           (matcha-command-to-normalized-title
            'matcha-prettier-or-indent-region-or-buffer)
           "Prettier Or Indent Region Or Buffer"))
  (should (string-equal
           (matcha-command-to-normalized-title
            'gofmt)
           "Gofmt")))

;;; matcha-test.el ends here
