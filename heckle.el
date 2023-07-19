;;; heckle.el --- Case conversion library and utility functions for Emacs. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Ryan Faulhaber
;;
;; Author: Ryan Faulhaber <http://github.com/rfaulhaber>
;; Maintainer: Ryan Faulhaber <ryf@sent.as>
;; Created: October 26, 2020
;; Modified: October 26, 2020
;; Version: 0.1.0
;; Keywords: lisp strings
;; Homepage: https://github.com/rfaulhaber/heckle
;; Package-Requires: ((emacs "26.1") s)
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
(require 'rx)

(defun heckle-to-kebab (arg)
  arg)

(defconst heckle--case-options `(('pascal . heckle-to-pascal-case)
                                 ('camel . heckle-to-camel-case)
                                 ('snake . heckle-to-snake-case)
                                 ('screaming-snake . heckle-to-screaming-snake-case)
                                 ('kebab . heckle-to-kebab-case)
                                 ('screaming-kebab . heckle-to-screaming-kebab-case))
  "Alist of case names and their mapping functions.")

(defun heckle--split-on-different-case (str)
  "Breaks up a PascalCase or camelCase STR into a list of strings."
  (let ((str-list ())
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

(defun heckle--determine-case (str)
  "Determines case type of STR. Splits string appropriately, returning a list of words."
  (let ((case-fold-search nil))
    (cond
     ((string-match-p heckle--pascal-case-pattern str) (heckle--split-on-different-case str))
     ((string-match-p heckle--snake-case-pattern str) (split-string str "_"))
     ((string-match-p heckle--camel-case-pattern str) (heckle--split-on-different-case str))
     ((string-match-p heckle--screaming-snake-case-pattern str) (split-string str "_"))
     ((string-match-p heckle--kebab-case-pattern str) (split-string str "-"))
     ((string-match-p heckle--screaming-kebab-case-pattern str) (split-string str "-"))
     (nil nil))))

(defun heckle--bounds-of-word ()
  "Return the bounds of the current word."
  (let* ((start (point))
         (word (current-word))
         (end (+ start (length word))))
    (cons start end)))

(defun heckle--get-bounds-of-word ()
  "Gets bounds of THING at cursor."
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (heckle--bounds-of-word)))
         (text (string-trim-left (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (start (car bounds))
         (end (cdr bounds)))
    (list 'text text 'start start 'end end)))

(defun heckle--lower-first (word)
  (concat (downcase (substring word 0 1)) (substring word 1)))

(defun heckle-to-pascal-case (word)
  (let ((word-list (heckle--determine-case word)))
    (mapconcat 'capitalize word-list "")))

(defun heckle-to-camel-case (word)
  (let ((word-list (heckle--determine-case word)))
    (heckle--lower-first (mapconcat 'capitalize word-list ""))))

(defun heckle-to-snake-case (word)
  (let ((word-list (heckle--determine-case word)))
    (mapconcat 'heckle--lower-first word-list "_")))

(defun heckle-to-kebab-case (word)
  (let ((word-list (heckle--determine-case word)))
    (mapconcat 'heckle--lower-first word-list "-")))

(defun heckle-to-screaming-snake-case (word)
  (upcase (heckle-to-snake-case word)))

(defun heckle-to-screaming-kebab-case (word)
  (upcase (heckle-to-kebab-case word)))

(defun heckle--point-or-region-to-case (new-case)
  (let* ((word-info (heckle--get-bounds-of-word))
         (word (plist-get word-info 'text))
         (start (plist-get word-info 'start))
         (end (plist-get word-info 'end))
         (result (funcall new-case word)))
    (delete-region start end)
    (insert result)))

;;;###autoload
(defun heckle-word-to-pascal-case ()
  "Convert word at point or region to PascalCase."
  (interactive)
  (heckle--point-or-region-to-case 'heckle-to-pascal-case))

;;;###autoload
(defun heckle-word-to-camel-case ()
  "Convert word at point or region to camelCase."
  (interactive)
  (heckle--point-or-region-to-case 'heckle-to-camel-case))

;;;###autoload
(defun heckle-word-to-snake-case ()
  "Convert word at point or region to snake_case."
  (interactive)
  (heckle--point-or-region-to-case 'heckle-to-snake-case))

;;;###autoload
(defun heckle-word-to-screaming-snake-case ()
  "Convert word at point or region to SCREAMING_SNAKE_CASE."
  (interactive)
  (heckle--point-or-region-to-case 'heckle-to-screaming-snake-case))

;;;###autoload
(defun heckle-word-to-kebab-case ()
  "Convert word at point or region to kebab-case."
  (interactive)
  (heckle--point-or-region-to-case 'heckle-to-kebab-case))

;;;###autoload
(defun heckle-word-to-screaming-kebab-case ()
  "Convert word at point or region to SCREAMING-KEBAB-CASE."
  (interactive)
  (heckle--point-or-region-to-case 'heckle-to-screaming-kebab-case))

;;;###autoload
(defun heckle-convert-point-or-region ()
  "Convert the word in point or region to the selected case."
  (interactive)
  (let ((selection (ivy-read "Select case: " (mapcar
                                              (lambda (e)
                                                (car e)) heckle--case-options))))
    (heckle--point-or-region-to-case (cdr (assoc selection heckle--case-options)))))

(provide 'heckle)
;;; heckle.el ends here
