;;; ftable.el --- Fill a table to fit in n columns      -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021 Free Software Foundation, Inc.

;; Author: Yuan Fu <casouri@gmail.com>
;; Maintainer: Yuan Fu <casouri@gmail.com>
;; URL: https://github.com/casouri/ftable
;; Version: 1.0
;; Keywords: convenience, text, table
;; Package-Requires: ((emacs "26.0"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides some convenient commands for filling a table,
;; i.e., adjusting the layout of the table so it can fit in n columns.
;;
;; Commands provided:
;;
;; - ftable-fill       Fill the table at point
;; - ftable-reformat   Change the style of the table. For example, from
;;                     ASCII +--+--+ to Unicode ┌──┬──┐
;;                           |  |  |            │  │  │
;;                           +--+--+            └──┴──┘
;; - ftable-edit-cell  Edit the cell at point
;;
;; There are some limitations. Currently ftable doesn’t support tables
;; with compound cells (cells that span multiple rows/columns) because
;; they are more complicated to handle. If the need arises in the
;; future (unlikely), I might improve ftable to handle more complex
;; tables. Also, after filling, any manual line-break in a cell is
;; discarded.
;;
;; Customization:
;;
;; - ftable-fill-column

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'pcase)
;; (require 'fill) fill.el doesn’t have a provide form.

;;; Customization

(defgroup ftable nil
  "Fill (auto-layout) tables."
  :group 'text)

(defcustom ftable-fill-column fill-column
  "Basically `fill-column' for ftable."
  :local t
  :type 'number)

;; KLUDGE: There seems to be a bug preventing ftable-fill-column to be
;; set. (#44911)
(setq ftable-fill-column fill-column)

;;; Table structure

(cl-defstruct ftable
  "A table.

COLUMN-WIDTH-LIST  A list that records the width (in characters)
                   of each column.
MIN-WIDTH-LIST     A list that records the minimum width (in
                   characters) of each column.
CELL-MATRIX        A list of list of strings. Each string is a cell.
                   Cells don’t contain newlines.

Each cell is a string, the cell doesn’t contain newlines. Column
width can be smaller than the string length of a cell, in which
case means the line cell is filled to that width."
  column-width-list
  min-width-list
  matrix)

(cl-deftype ftable-cell () '(satisfies stringp))

(cl-defmethod ftable--row-count ((table ftable))
  "Return the number of rows in TABLE."
  (length (ftable-matrix table)))

(cl-defmethod ftable--column-count ((table ftable))
  "Return the number of columns in TABLE."
  (length (car (ftable-matrix table))))

;;; Parse
;;
;; Transforming between text and table structure

(defvar ftable-box-charset-alist
  '((ascii . "
+-++
| ||
+-++
+-++")
    (unicode . "
┌─┬┐
│ ││
├─┼┤
└─┴┘"))
  "An alist of (NAME . CHARSET).
A charset tells ftable how to parse the table. I.e., what are the
box drawing characters to use. Don’t forget the first newline.
NAME is the mnemonic for that charset.")

(defun ftable-box-char (code charset)
  "Return a specific box drawing character in CHARSET.

Return a string. CHARSET should be like `ftable-box-char-set'.
Mapping between CODE and position:

    ┌┬┐     123
    ├┼┤ <-> 456
    └┴┘     789
   
    ┌─┐     1 H 3    H: horizontal
    │ │ <-> V   V    V: vertical
    └─┘     7 H 9

Examples:

    (ftable-box-char 'h charset) => \"─\"
    (ftable-box-char 2 charset)  => \"┬\""
  (let ((index (pcase code
                 ('h 10)
                 ('v 11)
                 ('n 12)
                 ('s 13)
                 (_ code))))
    
    (char-to-string
     (aref charset ;        1 2 3 4  5  6  7  8  9  H V N S
           (nth index '(nil 1 3 4 11 13 14 16 18 19 2 6 0 7))))))

;; Check `ftable-box-char' with the following form, you should see:
;; ┌─┬┐
;; │ ││
;; ├─┼┤
;; └─┴┘
;; (dolist (code '(
;;                 1 h 2 3 n
;;                 v s v v n
;;                 4 h 5 6 n
;;                 7 h 8 9 n))
;;   (insert (ftable-box-char code (cdadr ftable-box-charset-alist))))

(define-error 'ftable-parse-error "Error parsing table")

(cl-defmethod ftable--parse-to ((table-type (eql ftable)) text
                                &optional box-charset)
  "Parse TEXT into a table of TABLE-TYPE.
For BOX-CHARSET, see documentation of `ftable-box-charset-alist'.
It defaults to the first charset."
  (ignore table-type)
  ;; TODO Handle parse error.
  (let ((charset (or box-charset (cdar ftable-box-charset-alist)))
        line-list
        matrix
        buffer)
    ;; TEXT:
    ;; ┌──┬─┐
    ;; │ab│c│
    ;; ├──┼─┤
    ;; │de│f│
    ;; │gh│i│
    ;; └──┴─┘
    ;; 1. Split into lines.
    (setq line-list (split-string text "\n"))
    ;; LINE-LIST:
    ;; ("┌──┬─┐" "│ab│c│" "├──┼─┤" "│de│f│" "│gh│i│" "└──┴─┘")
    ;;
    ;; 2. Group each line into columns.
    (dolist (line line-list)
      (setq line (string-trim line))
      (if (or (string-prefix-p (ftable-box-char 1 charset) line)
              (string-prefix-p (ftable-box-char 4 charset) line)
              (string-prefix-p (ftable-box-char 7 charset) line))
          ;; Delimiter line, i.e. ┌──┬─┐, ├──┼─┤, etc.
          (progn (when buffer
                   (push (reverse buffer) matrix))
                 (setq buffer nil))
        (push (ftable--tokenize-line line charset) buffer)))
    (setq matrix (reverse matrix))
    ;; Sanity check.
    (when (not (ftable--check-dimension matrix))
      (signal 'ftable-parse-error '("Dimension mismatch")))
    ;; MATRIX:
    ;; ((("ab" "c")) (("de" "f") ("gh" "i")))
    ;;
    ;; 3. Merge lines that belongs to the same row.
    (setq matrix (mapcar #'ftable--merge-lines matrix))
    ;; MATRIX:
    ;; (("ab" "c") ("de gh" "f i"))
    (make-ftable
     :column-width-list
     (mapcar (lambda (column)
               (apply #'max (mapcar #'string-width column)))
             (cl-loop for n from 0 to (1- (length (car matrix)))
                      collect (ftable--nth-column n matrix)))
     :min-width-list
     (ftable--min-column-width matrix)
     :matrix matrix)))

(defun ftable--check-dimension (matrix)
  "Check that the dimension of MATRIX is correct.
Correct dimension means each row has the same number of columns.
Return t if the dimension is correct, nil if not."
  (let* ((matrix (apply #'append matrix))
         (first-row-column-count (length (car matrix))))
    (cl-loop for row in (cdr matrix)
             if (not (eq first-row-column-count (length row)))
             return nil
             finally return t)))

(defun ftable--tokenize-line (text-line box-charset)
  "Tokenize TEXT-LINE into a list of tokens.

Each token belongs to a cell. I.e.,

    (ftable--tokenize-line \"│a│b│c│\") => (\"a\" \"b\" \"c\")

BOX-CHARSET is the same as in `ftable--parse-to'.

Assumes each line begines with box drawing characters, i.e., no
white space characters."
  (mapcar #'string-trim
          (split-string (string-trim
                         text-line
                         (ftable-box-char 'v box-charset)
                         (ftable-box-char 'v box-charset))
                        (ftable-box-char 'v box-charset))))

(defun ftable--merge-lines (line-list)
  "Merge lines in LINE-LIST together.

    (ftable--merge-lines '((\"1\" \"2\" \"3\") (\"a\" \"b\" \"c\")))
 => (\"1 a\" \"2 b\" \"3 c\")

Assumes each line in LINE-LIST has the same length."
  (let (row)
    ;; Initialize ROW.
    (dotimes (_ (length (nth 0 line-list)))
      (push "" row))
    ;; Append cell contents.
    (dolist (line line-list)
      (dotimes (col-idx (length line))
        (setf (nth col-idx row)
              (concat (nth col-idx row) " "
                      (nth col-idx line)))))
    (mapcar #'string-trim row)))

(defun ftable--nth-column (n matrix)
  "Return the Nth column of MATRIX."
  (mapcar (lambda (row) (nth n row)) matrix))

(defun ftable--min-column-width (matrix)
  "Return the minimum width of each column in MATRIX."
  (with-temp-buffer
    (mapcar (lambda (column)
              (cl-loop for cell in column
                       maximize
                       (progn
                         (erase-buffer)
                         (insert cell)
                         (let ((fill-column 1))
                           (fill-region-as-paragraph
                            (point-min) (point-max)))
                         (ftable--max-line-width))))
            (cl-loop for col from 0 to (1- (length (car matrix)))
                     collect (ftable--nth-column col matrix)))))

;;; Fill

(cl-defmethod ftable--fill ((table ftable) table-max-width)
  "Return a new TABLE that fits in TABLE-MAX-WIDTH.
Try to fit in TABLE-MAX-WIDTH, if not possible, return the
mininum width table."
  (let ((desired-width table-max-width)
        table-height)
    (when (< table-max-width (ftable--min-width table))
      (setq desired-width (ftable--min-width table)))
    ;; While we haven’t satisfied the requirement and there is still
    ;; room for improvement:
    (while (< desired-width (ftable--width table))
      (setq table-height (ftable--height table))
      (let ((candidate-list
             ;; A list of (delta of height . new table).
             (cl-loop
              for col = 0 then (1+ col)
              for col-width in (ftable-column-width-list table)
              for min-width in (ftable-min-width-list table)
              if (> col-width min-width)
              collect
              (let ((new-table
                     (ftable--shrink-column table col 1)))
                (cons (- (ftable--height new-table)
                         table-height)
                      new-table)))))
        (if (= 0 (length candidate-list))
            (debug))
        (setq table
              (if (< (length candidate-list) 2)
                  (cdar candidate-list)
                (cdr
                 (cl-reduce
                  ;; Find argmin(delta of height).
                  (lambda (a b)
                    (if (< (car a) (car b)) a b))
                  candidate-list))))))
    table))

(cl-defmethod ftable--width ((table ftable))
  "Return the width of TABLE in characters.
This width includes all the box drawing characters."
  (let ((lst (ftable-column-width-list table)))
    (+ (apply #'+ lst)
       ;; Plus the horizontal bars.
       (1+ (length lst))
       ;; Plus one space padding for each column.
       (1+ (length lst)))))

(cl-defmethod ftable--min-width ((table ftable))
  "Return the smallest possible width of TABLE."
  (let ((lst (ftable-min-width-list table)))
    (+ (apply #'+ lst)
       ;; Plus the horizontal bars.
       (1+ (length lst))
       ;; Plus one space padding for each column.
       (1+ (length lst)))))

(cl-defmethod ftable--height ((table ftable))
  "Return the height of TABLE in chracters.
This height includes all the box drawing characters."
  (let ((width-list (ftable-column-width-list table))
        (matrix (ftable-matrix table)))
    (+ (cl-loop for row in matrix
                sum (ftable--row-height row width-list))
       (1+ (length (ftable-matrix table))))))

(defun ftable--row-height (row column-width-list)
  "Return the height of ROW.
Each cell in ROW is first filled according to COLUMN-WIDTH-LIST,
then the height is calculated."
  (with-temp-buffer
    (cl-loop
     for col from 0 to (1- (length row))
     ;; For each cell, fill the cell and count lines.
     maximize (let ((cell (nth col row))
                    (width (nth col column-width-list)))
                (erase-buffer)
                (insert cell)
                (let ((fill-column width))
                  (fill-region-as-paragraph (point-min) (point-max))
                  (count-lines (point-min) (point-max)))))))

(cl-defmethod ftable--shrink-column ((table ftable) n step)
  "Shrink column N of TABLE by STEP character.
Return a new table with shrinked column."
  (let ((width-list (ftable-column-width-list table))
        (min-list (ftable-min-width-list table))
        (matrix (ftable-matrix table)))
    (setf (nth n width-list)
          (- (nth n width-list) step))
    (make-ftable
     :column-width-list width-list
     :min-width-list min-list
     :matrix matrix)))

(defun ftable--max-line-width ()
  "Return the maximum line width in buffer."
  (apply #'max
         (mapcar #'string-width
                 (split-string (buffer-string) "\n"))))

;;; Unparse

(cl-defmethod ftable--unparse ((table ftable) &optional box-charset)
  "Export TABLE to text form.
BOX-CHARSET is the same as in `ftable--parse-to'."
  (let ((charset (or box-charset (cdar ftable-box-charset-alist)))
        (matrix (ftable-matrix table))
        (column-width-list (ftable-column-width-list table)))
    ;; MATRIX:
    ;; (("abc def" "123")
    ;;  ("ghi" "m"))
    ;;
    ;; WIDTH-LIST:
    ;; (3 3)
    ;;
    ;; 1. Split each row into lines.
    (setq matrix (mapcar (lambda (row)
                           (ftable--split-row row column-width-list))
                         matrix))
    ;; MATRIX:
    ;; ((("abc" "123")
    ;;   ("def" ""))
    ;;  (("ghi" "m")))
    ;;
    ;; We add a one-space padding to each column (only) when drawing
    ;; the table.
    (setq column-width-list (mapcar #'1+ column-width-list))
    (with-temp-buffer
      (cl-loop
       ;; Draw the top border.
       initially do
       (ftable--insert-grid-line column-width-list '(1 2 3) charset)
       for row in matrix
       ;; Draw lines of each row.
       do (cl-loop
           for line in row do
           (cl-loop initially do
                    (insert (ftable-box-char 'v charset))
                    for column in line
                    for width in column-width-list
                    do (insert (ftable--pad-to column width))
                    do (insert (ftable-box-char 'v charset))
                    finally do (insert "\n")))
       ;; Draw the separator line.
       (ftable--insert-grid-line column-width-list '(4 5 6) charset)
       ;; Draw the bottom border.
       finally do
       (progn
         (forward-line -1)
         (delete-region (line-beginning-position) (line-end-position))
         (ftable--insert-grid-line
          column-width-list '(7 8 9) charset)))
      
      (string-trim (buffer-string)))))

;; (defun ftable--transpose (matrix)
;;   "Transpose MATRIX."
;;   (cl-loop for col-idx from 0 to (1- (length (car matrix)))
;;            collect
;;            (cl-loop for row in matrix
;;                     collect (nth col-idx row))))

(defun ftable--insert-grid-line (column-width-list codeset charset)
  "Insert a grid line that separates cells vertically.
For example, ├──┼─┤. COLUMN-WIDTH-LIST is the one in `ftable'
struct. CODESET is a list of codes that corresponds to the left,
middle and right box drawing character codes to pass to
`ftable-box-char'. It can be (1 2 3), (4 5 6), or (7 8 9).
CHARSET is the same as BOX-CHARSET in `ftable--parse'."
  (let ((left (ftable-box-char (nth 0 codeset) charset))
        (middle (ftable-box-char (nth 1 codeset) charset))
        (right (ftable-box-char (nth 2 codeset) charset)))
    (cl-loop
     initially do (insert left)
     for width in column-width-list
     do (dotimes (_ width) (insert (ftable-box-char 'h charset)))
     do (insert middle)
     finally do (progn (backward-delete-char 1)
                       (insert right "\n")))))

(defun ftable--split-row (row column-width-list)
  "Split ROW into several lines according to COLUMN-WIDTH-LIST.
This is the opposite of `ftable--merge-lines'.

Return value has the form of:

    ((abc 123 ...) (def 456 ...) ...)

which corresponds to

    |abc|123|...|
    |def|456|...|
    ..."
  (let (line-count line-list line)
    (with-temp-buffer
      (setq row (cl-loop
                 for cell in row
                 for width in column-width-list
                 collect (progn
                           (erase-buffer)
                           (insert cell)
                           (let ((fill-column width))
                             (fill-region-as-paragraph
                              (point-min) (point-max)))
                           (split-string (string-trim (buffer-string))
                                         "\n"))))
      (setq line-count (apply #'max (mapcar #'length row)))
      (dotimes (idx line-count)
        (setq line nil)
        (dolist (cell row)
          (push (or (nth idx cell) "") line))
        (push (reverse line) line-list))
      (reverse line-list))))

(defun ftable--pad-to (text width)
  "Append padding to TEXT until it is WIDTH characters long.
Return a new string."
  (if (< (string-width text) width)
      (concat text (make-vector (- width (string-width text)) ?\s))
    text))

;;; Convenience

;;;###autoload
(defun ftable-fill ()
  "Fill the table (or paragraph) at point."
  (interactive)
  (pcase-let ((`(,text ,beg ,end ,cell-p ,tablep , charset)
               (ftable--table-info)))
    (if tablep
        (ftable--replace-text
         beg end text
         (string-trim
          (ftable--unparse
           (ftable--fill (ftable--parse-to
                          'ftable text charset)
                         ftable-fill-column)
           charset))
         (when cell-p
           #'table-recognize-region))
      (fill-paragraph))))

;;;###autoload
(defun ftable-edit-cell ()
  "Edit the cell at point."
  (interactive)
  (pcase-let* ((pt (point))
               (p-column (- (point) (line-beginning-position)))
               (`(,text ,beg ,end ,cell-p,tablep ,charset)
                (ftable--table-info))
               (x -1)
               ;; If these two characters are the same, we will count
               ;; one extra.
               (y (if (equal (ftable-box-char 3 charset)
                             (ftable-box-char 6 charset))
                      -1 0)))
    (if (not tablep)
        (user-error "There is no table at point")
      (save-excursion
        (goto-char beg)
        ;; Parse out the coordinate of the cell at point.
        (while (search-forward
                (format "%s\n" (ftable-box-char 6 charset))
                pt t)
          (cl-incf y))
        (while (search-forward
                (ftable-box-char 'v charset)
                (+ (line-beginning-position) p-column) t)
          (cl-incf x)))
      ;; Prompt user to edit.
      (let* ((table (ftable--parse-to 'ftable text charset))
             (cell (nth x (nth y (ftable-matrix table))))
             (new-cell (read-string "Edit: " cell)))
        (setf (nth x (nth y (ftable-matrix table))) new-cell)
        ;; Apply change.
        (ftable--replace-text
         beg end text
         (ftable--unparse
          (ftable--fill table ftable-fill-column)
          charset)
         (when cell-p
           #'table-recognize-region))))))

;;;###autoload
(defun ftable-reformat (style)
  "Change box drawing STYLE for table at point.
STYLE can be ’ascii or ’unicode."
  (interactive (list (intern
                      (downcase
                       (completing-read "Style: "
                                        '("ASCII" "Unicode"))))))
  (pcase-let ((`(,text ,beg ,end ,cell-p ,tablep ,charset)
               (ftable--table-info)))
    (if (not tablep)
        (user-error "There is no table at point")
      (ftable--replace-text
       beg end text
       (ftable--unparse
        ;; We still need to fill the table, otherwise it will be
        ;; the widest table layout.
        (ftable--fill
         (ftable--parse-to 'ftable text charset)
         ftable-fill-column)
        (alist-get style ftable-box-charset-alist))
       (when cell-p
         #'table-recognize-region)))))

(defun ftable--at-table-p ()
  "Return non-nil if point is in a table."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (member (char-to-string (char-after))
            (append
             (cl-loop for elt in ftable-box-charset-alist
                      for charset = (cdr elt)
                      collect (ftable-box-char 1 charset)
                      collect (ftable-box-char 4 charset)
                      collect (ftable-box-char 7 charset)
                      collect (ftable-box-char 'v charset))))))

(defun ftable--beginning-of-table ()
  "Go backward to the beginning of the table at point.
Assumes point is on a table."
  ;; This implementation allows non-table lines before a table, e.g.,
  ;; #+latex: xxx
  ;; |------+----|
  (when (ftable--at-table-p)
    (beginning-of-line))
  (while (and (< (point-min) (point))
              (ftable--at-table-p))
    (forward-line -1))
  (unless (ftable--at-table-p)
    (forward-line 1)))

(defun ftable--end-of-table ()
  "Go forward to the end of the table at point.
Assumes point is on a table."
  (let ((start (point)))
    (when (ftable--at-table-p)
      (beginning-of-line))
    (while (and (< (point) (point-max))
                (ftable--at-table-p))
      (forward-line 1))
    (unless (<= (point) start)
      (skip-chars-backward "\n"))
    (when (< (point) start)
      (error "End of table goes backwards"))))

(defun ftable--table-info ()
  "Return (TEXT BEG END TABLE-CELL-P TABLEP CHARSET).
TEXT is the table’s text. BEG and END are the beginning and end
position of the table, not including any newlines. TABLE-CELL-P
is t if the table is managed by table.el. TABLEP is t if point is
on a table, nil if not. CHARSET is the box drawing charset used
by the table (if there is a table). \(See
`ftable-box-charset-alist'.)"
  (let* ((beg (save-excursion
                (ftable--beginning-of-table)
                (point)))
         (end (save-excursion
                (ftable--end-of-table)
                (point)))
         (text (buffer-substring-no-properties beg end))
         (table-cell-p (text-property-any beg end 'table-cell t)))
    (append (list text beg end table-cell-p)
            (cl-loop for charset
                     in (mapcar #'cdr ftable-box-charset-alist)
                     if (equal (substring text 0 1)
                               (ftable-box-char 1 charset))
                     return (list t charset)
                     finally return (list nil nil)))))

(defun ftable--replace-text (beg end text new-text &optional fn)
  "Replace TEXT between BEG and END with NEW-TEXT.
If FN non-nil, run it with the new BEG and END after replacing
the text. I.e., (FN BEG END)."
  (unless (equal text new-text)
    (let ((p (point)))
      (delete-region beg end)
      (insert new-text)
      (setq end (point))
      ;; Go back to roughly where we were.
      (goto-char p)
      (when fn (funcall fn beg end)))))

;;; Test

(with-eval-after-load 'ert
  (ert-deftest ftable--misc-test ()
    (let ((text (string-trim "
┌──┬─┐
│ab│c│
├──┼─┤
│de│f│
│gh│i│
└──┴─┘")))
      (should (equal
               (ftable--parse-to 'ftable text
                                 (cdadr ftable-box-charset-alist))
               (make-ftable
                :column-width-list '(5 3)
                :min-width-list '(2 1)
                :matrix '(("ab" "c") ("de gh" "f i"))))))
    ;; ftable--tokenize-line
    (should (equal (mapcar (lambda (x)
                             (ftable--tokenize-line
                              x (cdadr ftable-box-charset-alist)))
                           '( "│ab│c│" "│de│f│" "│gh│i│"))
                   '(("ab" "c") ("de" "f") ("gh" "i") )))
    (should (equal (ftable--tokenize-line
                    "|fgh|  | z|" (cdar ftable-box-charset-alist))
                   '("fgh" "" "z")))
    ;; ftable--merge-lines
    (should (equal (mapcar #'ftable--merge-lines
                           '((("ab" "c")) (("de" "f") ("gh" "i"))))
                   '(("ab" "c") ("de gh" "f i"))))
    ;; ftable--nth-column
    (should (equal (ftable--nth-column 1 '((1 2 3) (4 5 6) (7 8 9)))
                   '(2 5 8)))
    ;; ftable--row-height
    (should (equal (ftable--row-height '("ab c" "def" "ghi") '(2 3 3))
                   2))
    ;; ftable--split-row
    (should (equal (ftable--split-row '("abc de" "12" "xy z")
                                      '(3 2 2))
                   '(("abc" "12" "xy") ("de" "" "z"))))
    ;; ftable--pad-to
    (should (equal (ftable--pad-to "123" 5)
                   "123  "))))


(provide 'ftable)

;;; ftable.el ends here
