;; -*- lexical-binding: t; -*-

(load-file "caseconv.el")

(ert-deftest split-splits-on-different-cases ()
  (let ((result (caseconv--split-on-different-case "fooBarBaz")))
    (should (eq t (listp result)))
    (should (equal result (list "foo" "Bar" "Baz")))))

(ert-deftest split-does-not-split-on-same-case ()
  (let ((result (caseconv--split-on-different-case "foobarbaz")))
    (should (string= "foobarbaz" (car result)))))
