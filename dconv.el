;;; dconv.el --- convert date strings

;; Copyright (C) 2008 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoulli.cc>
;; Created: 20081202
;; Updated: 20090118
;; Version: 0.0.1
;; Homepage: https://github.com/tarsius/dconv
;; Keywords: dates

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Convert date strings to "YYYYMMDD" format.

;; Note: The functions defined here commonly require a string as argument.
;;       But they also accept nil instead of a string in which case they
;;       simply return nil.  Convinient since this relieves callers from
;;       the need to verify the argument first which would otherwise be
;;       needed a lot in library `elx.el' for which this was written.
;; Note: Some libraries that might provide similar functionality:
;;       `date-calc.el', `strptime.el'.
;; Note: You shouldn't use this library yet - it has to be polished first.

;; TODO: Support more output formats.
;; TODO: Support more input formats, like:
;;       February 4, 2007
;;       14-Sep-1993
;;       22-11-2005 18:00 UTC
;;       different languages

;;; Code:

(defconst dconv--days
  '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defconst dconv--months
  '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defconst dconv--type1
  ;; Fri Oct 31 14:58:41 2008
  ;; Fri Oct 31 14:58:41 CET 2008
  ;; Fri Oct 31 14:58:41 2008 (Central European Time)
  (concat (regexp-opt dconv--days) " "
	  (regexp-opt dconv--months t) " "
	  "\\([ 0-3][0-9]\\)"
	  "[ 0-2][0-9]+:[0-5][0-9]:[0-5][0-9] "
	  "\\([a-zA-Z]+ \\)?"
	  "\\([0-9]\\{4,\\}\\)"))

(defconst dconv--type2
  ;; 2008/10/31  ||  2008-10-31  ||  2008.10.31
  "\\([0-9]\\{4,\\}\\)[-/\\.]\\([0-9]\\{1,2\\}\\)[-/\\.]\\([0-9]\\{1,2\\}\\)")

(defconst dconv--type3
  ;; 30 Oct 2008  ||  Oct 2008  ||  2008
  (concat "\\(\\([0-9]\\{1,2\\}\\) \\)?"
	  "\\(" (regexp-opt dconv--months t) " \\)?"
	  "\\([0-9]\\{4,\\}\\)"))

(defun dconv-convert-month (month)
  "Convert MONTH to \"MM\" format."
  (when month
    (setq month (replace-regexp-in-string " " "" month))
    (save-match-data
      (when (string-match (regexp-opt dconv--months) month)
	(setq month (number-to-string
		     (- 13 (length (member month dconv--months)))))))
    (if (= 1 (length month))
	(concat "0" month)
      month)))

(defun dconv-convert-day (day)
  "Convert DAY to \"DD\" format."
  (when day
    (setq day (replace-regexp-in-string " " "" day))
    (if (= 1 (length day))
	(concat "0" day)
      day)))

(defun dconv-convert-date (date)
  "Convert DATE to \"YYYYMMDD\" format."
  (cond ((not date) nil)
	((string-match dconv--type1 date)
	 (concat (match-string 4 date)
		 (dconv-convert-month (match-string 1 date))
		 (dconv-convert-day   (match-string 2 date))))
	((string-match dconv--type2 date)
	 (concat (match-string 1 date)
		 (dconv-convert-month (match-string 2 date))
		 (dconv-convert-day   (match-string 3 date))))
	((string-match dconv--type3 date)
	 (concat (match-string 5 date)
		 (dconv-convert-month (match-string 4 date))
		 (dconv-convert-day   (match-string 2 date))))))

(provide 'dconv)
;;; dconv.el ends here
