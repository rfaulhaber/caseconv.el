;;; caseconv.el --- Package for converting between cases -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Ryan Faulhaber
;;
;; Author: Ryan Faulhaber <http://github.com/rfaulhaber>
;; Maintainer: Ryan Faulhaber <ryan@sys9.net>
;; Created: July 15, 2020
;; Modified: July 15, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/rfaulhaber/caseconv
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(defgroup caseconv nil
  "Minor mode for changing cases"
  :group 'tools)

(defvar caseconv-modify-case-fold-search t
  "Tells caseconv whether or not it can modify the 'case-fold-search' value.
If nil, this value will not be modified, though this package may not work right!")

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
  (rx bol (one-or-more (in "a-z?!/")) (zero-or-more "-" (one-or-more (in "a-z0-9"))) eol))

(defun caseconv--determine-case (str)
  "Determines case type of STR. Splits string appropriately, returning a list of words."
  (cond
   ((string-match-p caseconv--pascal-case-pattern str) (split-string str ""))
   ((string-match-p caseconv--snake-case-pattern str) 'snake)
   ((string-match-p caseconv--snake-case-pattern str) 'snake)
   )
  )

(provide 'caseconv)
;;; caseconv.el ends here
