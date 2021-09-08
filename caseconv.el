;;; caseconv.el --- Package for converting between cases -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Ryan Faulhaber
;;
;; Author: Ryan Faulhaber <http://github.com/rfaulhaber>
;; Maintainer: Ryan Faulhaber <ryf@sent.as>
;; Created: October 26, 2020
;; Modified: October 26, 2020
;; Version: 0.1.0
;; Keywords:
;; Homepage: https://github.com/rfaulhaber/caseconv
;; Package-Requires: ((emacs "24.4") s subr-x)
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  This package provides both interactive and non-interactive Lisp functions
;;  for converting between six different types of "cases":
;;
;; - camelCase
;; - PascalCase
;; - kebab-case
;; - snake_case
;; - SCREAMING_SNAKE_CASE
;; - SCREAMING-KEBAB-CASE
;;
;;
;;; Code:

(require 's)
(require 'subr-x)

(defgroup caseconv nil
  "Minor mode for changing cases"
  :group 'tools)

(defconst caseconv--pascal-case-pattern
  (rx bol (one-or-more (any "A-Z") (one-or-more (in "a-z0-9"))) eol))
(defconst caseconv--snake-case-pattern
  (rx bol (one-or-more (in "a-z")) (zero-or-more "_" (one-or-more (in "a-z0-9"))) eol))
(defconst caseconv--camel-case-pattern
  (rx bol (one-or-more (any "a-z")) (one-or-more (any "A-Z") (one-or-more (any "a-z0-9"))) eol))
(defconst caseconv--screaming-snake-case-pattern
  (rx bol (one-or-more (in "A-Z")) (zero-or-more "_" (one-or-more (in "A-Z0-9"))) eol))
(defconst caseconv--kebab-case-pattern
  (rx bol (one-or-more (in "a-z?!/")) (zero-or-more "-" (one-or-more (in "a-z0-9"))) eol))
(defconst caseconv--screaming-kebab-case-pattern
  (rx bol (one-or-more (in "A-Z?!/")) (zero-or-more "-" (one-or-more (in "A-Z0-9"))) eol))

(defconst caseconv--case-options `(("PascalCase" . caseconv-to-pascal-case)
                                   ("camelCase" . caseconv-to-camel-case)
                                   ("snake_case" . caseconv-to-snake-case)
                                   ("SCREAMING_SNAKE_CASE" . caseconv-to-screaming-snake-case)
                                   ("kebab-case" . caseconv-to-kebab-case)
                                   ("SCREAMING-KEBAB-CASE" . caseconv-to-screaming-kebab-case))
  "Alist of case names and their mapping functions.")

(defun caseconv--split-on-different-case (str)
  "Breaks up a PascalCase or camelCase STR into a list of strings."
  (let ((str-list '())
        (current-str (substring str 0 1))
        (index 1))
    (while (<= index (length str))
      (let ((current-char (substring str (- index 1) index)))
        (unless (= index 1)
          (let ((last-char (substring str (- index 2) (- index 1))))
            (cond
             ((and (s-capitalized? current-char) (not (s-capitalized? last-char)))
              (progn
                (setq str-list (cons current-str str-list))
                (setq current-str current-char)))
             ((and (s-capitalized? last-char) (not (s-capitalized? current-char)))
              (setq current-str (concat current-str current-char)))
             (t
              (setq current-str (concat current-str current-char)))))))
      (setq index (+ 1 index)))
    (setq str-list (cons current-str str-list))
    (reverse str-list)))

(defun caseconv--determine-case (str)
  "Determines case type of STR. Splits string appropriately, returning a list of words."
  (let ((case-fold-search nil))
    (cond
     ((string-match-p caseconv--pascal-case-pattern str) (caseconv--split-on-different-case str))
     ((string-match-p caseconv--snake-case-pattern str) (split-string str "_"))
     ((string-match-p caseconv--camel-case-pattern str) (caseconv--split-on-different-case str))
     ((string-match-p caseconv--screaming-snake-case-pattern str) (split-string str "_"))
     ((string-match-p caseconv--kebab-case-pattern str) (split-string str "-"))
     ((string-match-p caseconv--screaming-kebab-case-pattern str) (split-string str "-"))
     (nil nil))))

(defun caseconv--bounds-of-word ()
  "Return the bounds of the current word."
  (let* ((start (point))
         (word (current-word))
         (end (+ start (length word))))
    (cons start end)))

(defun caseconv--get-bounds-of-word ()
  "Gets bounds of THING at cursor."
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (caseconv--bounds-of-word)))
         (text (string-trim-left (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (start (car bounds))
         (end (cdr bounds)))
    (list 'text text 'start start 'end end)))

(defun caseconv--lower-first (word)
  (concat (downcase (substring word 0 1)) (substring word 1)))

(defun caseconv-to-pascal-case (word)
  (let ((word-list (caseconv--determine-case word)))
    (mapconcat 'capitalize word-list "")))

(defun caseconv-to-camel-case (word)
  (let ((word-list (caseconv--determine-case word)))
    (caseconv--lower-first (mapconcat 'capitalize word-list ""))))

(defun caseconv-to-snake-case (word)
  (let ((word-list (caseconv--determine-case word)))
    (mapconcat 'caseconv--lower-first word-list "_")))

(defun caseconv-to-kebab-case (word)
  (let ((word-list (caseconv--determine-case word)))
    (mapconcat 'caseconv--lower-first word-list "-")))

(defun caseconv-to-screaming-snake-case (word)
  (upcase (caseconv-to-snake-case word)))

(defun caseconv-to-screaming-kebab-case (word)
  (upcase (caseconv-to-kebab-case word)))

(defun caseconv--point-or-region-to-case (new-case)
  (let* ((word-info (caseconv--get-bounds-of-word))
         (word (plist-get word-info 'text))
         (start (plist-get word-info 'start))
         (end (plist-get word-info 'end))
         (result (funcall new-case word)))
    (delete-region start end)
    (insert result)))

(defun caseconv-convert-point-or-region ()
  "Converts the word in point or region to the selected case."
  (interactive)
  (let ((selection (ivy-read "Select case: " (mapcar
                                              (lambda (e)
                                                (car e)) caseconv--case-options))))
    (caseconv--point-or-region-to-case (cdr (assoc selection caseconv--case-options)))))

(provide 'caseconv)
;;; caseconv.el ends here
