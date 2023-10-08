
;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; Copyright (C) 2019, Dmitry Ignatiev <lovesan.ru at gmail.com>

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package #:bike-internals)

(deftype simple-character-string ()
  "A simple one-dimensional array which element type is CHARACTER"
  '(simple-array character (*)))

(declaim (inline %base-string-to-string))
(defun %base-string-to-string (string &optional upcase)
  (declare (type simple-base-string string))
  (let* ((length (length string))
         (result (make-string length)))
    (if upcase
      (dotimes (i length)
        (setf (schar result i) (char-upcase (schar string i))))
      (dotimes (i length)
        (setf (schar result i) (schar string i))))
    result))

(declaim (inline %maybe-string-upcase))
(defun %maybe-string-upcase (string)
  (declare (type simple-character-string string))
  (let ((len (length string)))
    (if (dotimes (i len t)
          (when (lower-case-p (schar string i))
            (return nil)))
      string
      (let ((result (copy-seq string)))
        (nstring-upcase result)
        result))))

(defun simple-character-string (designator)
  (declare (type string-designator designator))
  "Converts a STRING-DESIGNATOR to a SIMPLE-CHARACTER-STRING."
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (flet ((from-base-string (s) (%base-string-to-string s)))
      (etypecase designator
        (simple-character-string designator)
        (symbol (let ((name (symbol-name designator)))
                  (if (typep name 'simple-character-string)
                    name
                    (from-base-string name))))
        (simple-base-string (from-base-string designator))
        (character
         (make-array 1 :element-type 'character
                       :initial-element designator))))))

(declaim (inline make-simple-character-string))
(defun make-simple-character-string (length)
  (declare (type (integer 0 #.(1- array-total-size-limit)) length))
  "Allocates a SIMPLE-CHARACTER-STRING that can hold LENGTH characters."
  (make-array length :element-type 'character))

(defun simple-character-string-upcase (designator)
  (declare (type string-designator designator))
  "Converts a STRING-DESIGNATOR to an upper-case SIMPLE-CHARACTER-STRING."
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (flet ((maybe-upcase (s) (%maybe-string-upcase s))
           (from-base-string (s) (%base-string-to-string s t)))
      (etypecase designator
        (simple-character-string (maybe-upcase designator))
        (symbol (let ((name (symbol-name designator)))
                  (if (typep name 'simple-character-string)
                    (maybe-upcase name)
                    (from-base-string name))))
        (simple-base-string (from-base-string designator))
        (base-char
         (make-array 1 :element-type 'character
                       :initial-element (char-upcase designator)))
        (character
         (make-array 1 :element-type 'character
                       :initial-element (char-upcase designator)))))))

(defconstant +ascii-char-code-limit+ 128)

(defun whitespace-char-p (c)
  (declare (type character c))
  "Returns Non-NIL value in case of character being a white space character."
  (let ((code (char-code c)))
    (if (< code +ascii-char-code-limit+) ;; short path
      (or (= code #x0020)                 ;;  space
          (and (>= code #x0009)           ;;   or
               (<= code #x000D)))         ;;  \t..\r
      ;; assume that CL implementation supports unicode
      (case code
        ((#x0085  ;; NEXT LINE
          #x00A0  ;; NO-BREAK SPACE
          #x1680  ;; OGHAM SPACE MARK
          #x2000  ;; EN QUAD
          #x2001  ;; EM QUAD
          #x2002  ;; EN SPACE
          #x2003  ;; EM SPACE
          #x2004  ;; THREE-PER-EM SPACE
          #x2005  ;; FOUR-PER-EM SPACE
          #x2006  ;; SIX-PER-EM SPACE
          #x2007  ;; FIGURE SPACE
          #x2008  ;; PUNCTUATION SPACE
          #x2009  ;; THIN SPACE
          #x200A  ;; HAIR SPACE
          #x2028  ;; LINE SEPARATOR
          #x2029  ;; PARAGRAPH SEPARATOR
          #x202F  ;; NARROW NO-BREAK SPACE
          #x205F  ;; MEDIUM MATHEMATICAL SPACE
          #x3000) ;; IDEOGRAPHIC SPACE
         t)))))

(defun camel-case-string (designator &key capitalize
                                          (handle-underscores t)
                                          (handle-dot t)
                                          (handle-whitespace :strip)
                                          prepend-underscore
                                          (handle-percent :strip))
  (declare (type string-designator designator))
  "Converts a DESIGNATOR which must be a STRING-DESIGNATOR to a string
   replacing Lisp-style `naming-convention' with `camelCase'.

:CAPITALIZE - non-NIL value designates that the result should be capitalized
              as in `PascalCase'.
              In case of the value being equal to `:FORCE', this setting
              overrides `:HANDLE-PERCENT' behavior described below.

:HANDLE-UNDERSCORES - non-NIL value designates that `_' characters should be
                      stripped and treated as word separators.

:HANDLE-DOT - non-NIL value designates that `.' characters should be treated as
              word separators.

:HANDLE-WHITESPACE - non-NIL value designates that whitespace characters should be
                     treated as word separators.
                     Additionally, such characters will be stripped in case the value
                     equals to `:STRIP' keyword.

:PREPEND-UNDERSCORE - non-NIL value designates whether the result should include
                      a prepended underscore character, as in `_camelCase'.

:HANDLE-PERCENT - Value of `:STRIP' designates that starting `%' characters should
                  be stripped.
                  Other non-NIL value designates that starting `%' characters should
                  be replaced with underscores. In this case, string capitalization
                  is turned off unless the value of `:CAPITALIZE' equals to `:FORCE'.
"
  (let* ((str (simple-character-string designator))
         (len (length str))
         (res (make-simple-character-string (1+ len)))
         (i 0)
         (j 0)
         (c nil)
         (state :start))
    (flet ((next (next-state)
             (when (< i len) (incf i))
             (setf state next-state))
           (peek () (setf c (if (< i len) (schar str i) nil)))
           (upcase () (when c (setf c (char-upcase c))))
           (downcase () (when c (setf c (char-downcase c))))
           (add (next-state)
             (when c
               (setf (schar res j) c)
               (incf j)
               (when (< i len) (incf i)))
             (setf state next-state)))
      (when prepend-underscore
        (setf (schar res 0) #\_ j 1))
      (case handle-percent
        ((nil)) ;; do nothing
        (:strip (loop :while (and (< i len)
                                  (char= (schar str i) #\%))
                      :do (incf i)))
        (t (loop :while (and (< i len)
                             (char= (schar str i) #\%))
                 :do (setf (schar res i) #\_)
                     (unless (eq capitalize :force)
                       (setf capitalize nil))
                     (incf j)
                     (incf i))))
      (loop
        (case state
          (:start
           (peek)
           (cond
             ((null c) (return))
             ((char= c #\-) (next :start))
             ((char= c #\_)
              (if handle-underscores (next :start) (add :cont)))
             ((char= c #\.)
              (add (if handle-dot :start :cont)))
             ((and handle-whitespace (whitespace-char-p c))
              (if (eq handle-whitespace :strip)
                (next :start)
                (add :start)))
             (t (if capitalize (upcase) (downcase))
                (add :cont))))
          (:cont
           (peek)
           (cond
             ((null c) (return))
             ((char= c #\-) (next :break))
             ((char= c #\_)
              (if handle-underscores (next :break) (add :cont)))
             ((char= c #\.)
              (add (if handle-dot :break :cont)))
             ((and handle-whitespace (whitespace-char-p c))
              (setf capitalize t)
              (if (eq handle-whitespace :strip)
                (next :start)
                (add :start)))
             (t (downcase) (add :cont))))
          (:break
           (peek)
           (cond
             ((null c) (return))
             ((char= c #\-) (next :break))
             ((char= c #\_)
              (if handle-underscores (next :break) (add :cont)))
             ((char= c #\.)
              (add (if handle-dot :break :cont)))
             ((and handle-whitespace (whitespace-char-p c))
              (setf capitalize t)
              (if (eq handle-whitespace :strip)
                (next :start)
                (add :start)))
             (t (upcase) (add :cont)))))))
    (subseq res 0 j)))

(defun lisp-case-string (designator &key (case (readtable-case *readtable*))
                                         (handle-underscores t)
                                         (handle-whitespace t)
                                         (handle-dot t)
                                         suffix
                                         prefix)
  (declare (type string-designator designator)
           (type (or null string-designator) suffix prefix)
           (type (member :upcase :downcase :preserve :invert) case))
  "Converts a DESIGNATOR which must be a STRING-DESIGNATOR to a string
   in a Lisp-style `naming-convention' instead of a `camelCase' one.

:HANDLE-UNDERSCORES - non-NIL value designates that `_' characters should
                      be treated as word separators.

:HANDLE-DOT - non-NIL value designates that `.' characters should
              be treated as word separators.

:HANDLE-WHITESPACE - non-NIL value designates that whitespace characters should
                     be treated as word separators.

:PREFIX - either NIL or a string designator which would be prepended to the result.

:SUFFIX - either NIL or a string designator which would be appended to the result.
"
  (let* ((suffix (or (and suffix (simple-character-string suffix))
                     (make-simple-character-string 0)))
         (prefix (or (and prefix (simple-character-string prefix))
                     (make-simple-character-string 0)))
         (str (simple-character-string designator))
         (len (length str))
         (res (make-simple-character-string (+ (length prefix)
                                               (+ len  (floor len 2))
                                               (length suffix))))
         (res-len (length res))
         (i 0)
         (j 0)
         (c nil)
         (state :start))
    (labels ((ensure-size ()
               (unless (< j res-len)
                 (let* ((new-res-len (1+ (* res-len 2)))
                        (new-res (make-simple-character-string new-res-len)))
                   (replace new-res res)
                   (setf res-len new-res-len
                         res new-res))))
             (next (next-state)
               (when (< i len) (incf i))
               (setf state next-state))
             (peek () (setf c (if (< i len) (schar str i) nil)))
             (add (next-state)
               (when c
                 (ensure-size)
                 (setf (schar res j) c)
                 (incf j)
                 (when (< i len) (incf i)))
               (setf state next-state))
             (separator ()
               (ensure-size)
               (setf (schar res j) #\-)
               (incf j))
             (set-case ()
               (when c (setf c (ecase case
                                 (:upcase (char-upcase c))
                                 (:downcase (char-downcase c))
                                 (:preserve c)
                                 (:invert (cond ((upper-case-p c)
                                                 (char-downcase c))
                                                ((lower-case-p c)
                                                 (char-upcase c))
                                                (t c))))))))
      (dotimes (i (length prefix))
        (setf (schar res j) (schar prefix i))
        (incf j))
      (loop
        (case state
          (:start
           (peek)
           (cond
             ((null c) (return))
             ((and handle-whitespace (whitespace-char-p c))
              (next :start))
             ((and handle-underscores (char= c #\_))
              (next :start))
             ((and handle-dot (char= c #\.))
              (next :start))
             ((upper-case-p c)
              (set-case) (add :upcase))
             (t (set-case) (add :downcase))))
          (:upcase
           (peek)
           (cond
             ((null c) (return))
             ((and handle-whitespace (whitespace-char-p c))
              (next :break))
             ((and handle-underscores (char= c #\_))
              (next :break))
             ((and handle-dot (char= c #\.))
              (next :break))
             ((upper-case-p c)
              (set-case) (add :upcase))
             (t (set-case) (add :downcase))))
          (:downcase
           (peek)
           (cond
             ((null c) (return))
             ((and handle-whitespace (whitespace-char-p c))
              (next :break))
             ((and handle-underscores (char= c #\_))
              (next :break))
             ((and handle-dot (char= c #\.))
              (next :break))
             ((upper-case-p c)
              (set-case) (separator) (add :upcase))
             (t (set-case) (add :downcase))))
          (:break
           (peek)
           (cond
             ((null c) (return))
             ((and handle-whitespace (whitespace-char-p c))
              (next :break))
             ((and handle-underscores (char= c #\_))
              (next :break))
             ((and handle-dot (char= c #\.))
              (next :break))
             ((upper-case-p c)
              (set-case) (separator) (add :upcase))
             (t (set-case) (separator) (add :downcase))))))
      (dotimes (i (length suffix))
        (setf (schar res j) (schar suffix i))
        (incf j)))
    (subseq res 0 j)))

;;; vim: ft=lisp et
