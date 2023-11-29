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

(deftype line-callback ()
  "A function which is called on line processing"
  '(or symbol (function (simple-character-string ; buffer
                         array-index             ; start
                         array-index             ; end
                         t)                      ; trailing?
               (values))))

(defvar *default-string-buffer-capacity* 20
  "Default capacity for STRING-BUFFER")

(defconstant +max-string-buffer-capacity+ (1- array-total-size-limit))

(defstruct (string-buffer (:constructor %make-string-buffer (data cap len))
                          (:conc-name %sb-)
                          (:predicate string-buffer-p)
                          (:copier nil))
  "A character buffer"
  (data (make-simple-character-string *default-string-buffer-capacity*)
   :type simple-character-string)
  (cap *default-string-buffer-capacity* :type (integer 1 #.(1- array-total-size-limit)))
  (len 0 :type array-index))

(defstruct (line-buffer (:include string-buffer)
                        (:constructor %make-line-buffer (data cap len line-start pos seen-cr))
                        (:conc-name %lb-)
                        (:predicate line-buffer-p)
                        (:copier nil))
  "A line buffer"
  (line-start 0 :type array-index)
  (pos 0 :type array-index)
  (seen-cr nil :type boolean))

(declaim (inline make-simple-character-string))
(defun make-simple-character-string (length)
  (declare (type (integer 0 #.(1- array-total-size-limit)) length))
  "Allocates a SIMPLE-CHARACTER-STRING that can hold LENGTH characters."
  (make-array length :element-type 'character))

(declaim (inline sb-data))
(defun sb-data (string-buffer)
  (declare (type string-buffer string-buffer))
  "Retrieves data vector of a STRING-BUFFER."
  (%sb-data string-buffer))

(declaim (inline sb-capacity))
(defun sb-capacity (string-buffer)
  (declare (type string-buffer string-buffer))
  "Retrieves capacity of a STRING-BUFFER."
  (%sb-cap string-buffer))

(declaim (inline sb-length))
(defun sb-length (string-buffer)
  (declare (type string-buffer string-buffer))
  "Retrieves current length of a STRING-BUFFER."
  (%sb-len string-buffer))

(defun (setf sb-length) (new-length string-buffer)
  (declare (type string-buffer string-buffer)
           (type array-index new-length))
  "Sets new length for string-buffer"
  (with-accessors ((len %sb-len))
      string-buffer
    (when (> new-length len)
      (sb-ensure-capacity string-buffer new-length))
    (setf len new-length)))

(defun sb-string (string-buffer)
  (declare (type string-buffer string-buffer))
  "Retrieves the contents of a STRING-BUFFER as a string,
taking its length into account."
  (subseq (%sb-data string-buffer) 0 (%sb-len string-buffer)))

(declaim (inline lb-line-start))
(defun lb-line-start (line-buffer)
  (declare (type line-buffer line-buffer))
  "Retrieves current line start inside LINE-BUFFER."
  (%lb-line-start line-buffer))

(declaim (inline lb-position))
(defun lb-position (line-buffer)
  (declare (type line-buffer line-buffer))
  "Retrieves current position inside LINE-BUFFER."
  (%lb-pos line-buffer))

(declaim (inline lb-seen-cr-p))
(defun lb-seen-cr-p (line-buffer)
  (declare (type line-buffer line-buffer))
  "Returns non-NIL in case of previous character inside LINE-BUFFER was a #\Return."
  (%lb-seen-cr line-buffer))

(declaim (inline make-string-buffer))
(defun make-string-buffer (&optional (capacity *default-string-buffer-capacity*))
  (declare (type (integer 1 #.(1- array-total-size-limit)) capacity))
  "Allocates a string buffer which can hold CAPACITY characters initially."
  (%make-string-buffer (make-simple-character-string capacity)
                       capacity
                       0))

(declaim (inline make-line-buffer))
(defun make-line-buffer (&optional (capacity *default-string-buffer-capacity*))
  (declare (type (integer 1 #.(1- array-total-size-limit)) capacity))
  "Allocates a line buffer which can hold CAPACITY characters initially."
  (%make-line-buffer
   (make-simple-character-string capacity) capacity 0 0 0 nil))

(declaim (inline copy-string-buffer))
(defun copy-string-buffer (string-buffer)
  (declare (type string-buffer string-buffer))
  "Copies a STRING-BUFFER."
  (let* ((capacity (%sb-cap string-buffer))
         (data (%sb-data string-buffer))
         (length (%sb-len string-buffer))
         (new-data (make-simple-character-string capacity)))
    (replace new-data data :start1 0 :end1 length
                           :start2 0 :end2 length)
    (%make-string-buffer new-data capacity length)))

(declaim (inline copy-line-buffer))
(defun copy-line-buffer (line-buffer)
  (declare (type line-buffer line-buffer))
  "Copies a LINE-BUFFER."
  (let* ((capacity (%lb-cap line-buffer))
         (data (%lb-data line-buffer))
         (length (%lb-len line-buffer))
         (new-data (make-simple-character-string capacity))
         (line-start (%lb-line-start line-buffer))
         (pos (%lb-pos line-buffer))
         (seen-cr (%lb-seen-cr line-buffer)))
    (replace new-data data :start1 0 :end1 length
                           :start2 0 :end2 length)
    (%make-line-buffer new-data capacity length line-start pos seen-cr)))

(defun sb-ensure-capacity (string-buffer required-capacity)
  (declare (type string-buffer string-buffer)
           (type (integer 1 #.(1- array-total-size-limit)) required-capacity))
  "Ensures that a STRING-BUFFER can hold at least REQUIRED-CAPACITY characters."
  (with-accessors ((data %sb-data)
                   (capacity %sb-cap)
                   (len %sb-len))
      string-buffer
    (when (< capacity required-capacity)
      (let* ((new-capacity (max (1+ required-capacity)
                                (1+ (* 2 capacity))))
             (new-data (make-simple-character-string new-capacity)))
        (replace new-data data :start1 0 :end1 len
                               :start2 0 :end2 len)
        (setf capacity new-capacity
              data new-data)))
    capacity))

(defun sb-extend (string-buffer count)
  (declare (type string-buffer string-buffer)
           (type non-negative-fixnum count))
  "Extends length of a STRING-BUFFER by COUNT characters."
  (with-accessors ((capacity %sb-cap)
                   (len %sb-len))
      string-buffer
    (let* ((new-length (+ len count)))
      (when (> new-length +max-string-buffer-capacity+)
        (error "~a exceeds maximum string buffer capacity." new-length))
      (sb-ensure-capacity string-buffer new-length)
      (setf len new-length))
    len))

(defun sb-delete (string-buffer &key (start 0) end)
  (declare (type string-buffer string-buffer)
           (type array-index start)
           (type (or array-index null) end))
  "Removes characters from a STRING-BUFFER
 in the range from the START index and below the END index."
  (with-accessors ((len %sb-len)
                   (capacity %sb-cap)
                   (data %sb-data))
      string-buffer
    (unless end
      (setf end len))
    (cond ((or (> start len)
               (>= start end))
           ;; do nothing
           (values))
          ((= end len)
           (setf len start))
          (t (replace data data :start1 start
                                :start2 end :end2 len)
             (decf len (- end start)))))
  string-buffer)

(defun sb-insert-string (string-buffer string &key (start1 0)
                                                   end1
                                                   (start2 0)
                                                   end2)
  (declare (type string-buffer string-buffer)
           (type array-index start1 start2)
           (type (or null array-index) end1 end2)
           (type string string))
  "Inserts a STRING in the STRING-BUFFER."
  (with-accessors ((len %sb-len)
                   (data %sb-data))
      string-buffer
    (let* ((end1 (or end1 len))
           (end2 (or end2 (length string)))
           (string-len (- end2 start2))
           (end1 (min end1 (+ start1 string-len)))
           (insertion-len (- end1 start1)))
      (when (> insertion-len 0)
        (sb-extend string-buffer insertion-len)
        (replace data data :start1 end1
                           :start2 start1 :end2 len)
        (replace data string :start1 start1 :end1 end1
                             :start2 start2 :end2 end2))))
  string-buffer)

(defun sb-append-string (string-buffer string &key (start 0)
                                                   end)
  (declare (type string-buffer string-buffer)
           (type string string)
           (type array-index start)
           (type (or null array-index) end))
  "Appends a STRING to the end of STRING-BUFFER."
  (let* ((end (or end (length string)))
         (sb-len (%sb-len string-buffer))
         (string-len (- end start)))
    (when (> string-len 0)
      (sb-extend string-buffer string-len)
      (replace (%sb-data string-buffer) string
               :start1 sb-len
               :start2 start :end2 end))
    string-buffer))

(defun sb-append-line (string-buffer &optional (string "")
                                               (start 0)
                                               end)
  (declare (type string-buffer string-buffer)
           (type string string)
           (type array-index start)
           (type (or null array-index) end))
  "Appends a STRING followed by a #\Newline to a STRING-BUFFER."
  (sb-append-string string-buffer string :start start :end end)
  (sb-extend string-buffer 1)
  (with-accessors ((len %sb-len)
                   (data %sb-data))
      string-buffer
    (setf (schar data (1- len)) #\Newline))
  string-buffer)

(defun sb-append-format (string-buffer control-string &rest args)
  (declare (type string-buffer string-buffer)
           (type string control-string)
           (dynamic-extent args))
  "Appends a formatted string to STRING-BUFFER using FORMAT function."
  (sb-append-string string-buffer (apply #'format nil control-string args)))

(defun make-line-output-callback (stream)
  (declare (type stream stream))
  "Creates a LINE-CALLBACK which outputs lines to a STREAM."
  (labels ((line-output-callback (buffer start end trailing)
             (if trailing
               (write-string buffer stream :start start :end end)
               (write-line buffer stream :start start :end end))
             (force-output)))
    #'line-output-callback))

(defun process-string-lines
    (string &key (line-callback (make-line-output-callback *standard-output*))
                 (start 0)
                 end
                 (line-start start)
                 seen-cr
                 skip-trailing-line)
  (declare (type simple-character-string string)
           (type (or symbol (cons (eql setf) (cons symbol null)) line-callback)
                 line-callback)
           (type non-negative-fixnum line-start start)
           (type (or null non-negative-fixnum) end))
  "Processes lines of a STRING and applies a LINE-CALLBACK to each line.

:START - Designates starting position inside STRING.

:END - Process characters until this position inside STRING.

:LINE-START - Designates previous line start position inside STRING.

:SEEN-CR - Designates whether previous call to this function stumbled upon #\Return character.

:SKIP-TRAILING-LINE - When non-NIL, skips the last line."
  (let ((end (or end (length string)))
        (i start))
    (when (and seen-cr (> end start) (char= #\Newline (schar string 0)))
      (incf i)
      (incf line-start)
      (setf seen-cr nil))
    (loop :while (< i end) :do
      (let ((c (schar string i)))
        (case c
          ((#\Return #\Newline)
           (funcall line-callback string line-start i nil)
           (setf line-start (1+ i))
           (when (and (char= c #\Return)
                      (< line-start end)
                      (char= (schar string line-start) #\Newline))
             (incf line-start)
             (incf i)))))
      (incf i))
    (when (and (> end start) (char= (schar string (1- end)) #\Return))
      (setf seen-cr t))
    (unless (and skip-trailing-line (< line-start end))
      (funcall line-callback string line-start end t))
    (values string i line-start seen-cr)))

(defun lb-shift (line-buffer)
  (declare (type line-buffer line-buffer))
  "Removes characters from LINE-BUFFER up to the beginning of the current line."
  (with-accessors ((line-start %lb-line-start)
                   (pos %lb-pos)
                   (len %lb-len))
      line-buffer
    (unless (zerop line-start)
      (let ((data (%lb-data line-buffer))
            (left (- len line-start)))
        (unless (zerop left)
          (replace data data :start1 0 :end1 left
                             :start2 line-start :end2 len))
        (psetf line-start 0
               len left
               pos (- pos line-start)))))
  line-buffer)

(defun lb-process-lines (line-buffer line-callback &optional skip-trailing-line)
  (declare (type line-buffer line-buffer)
           (type (or symbol line-callback (cons (eql setf) (cons symbol null)))
                 line-callback))
  "Processes lines inside LINE-BUFFER using a LINE-CALLBACK.
LINE-BUFFER is destructively modified, and all the processed lines are removed from it.

SKIP-TRAILING-LINE - When non-NIL, skips the last line unless it ends with #\Newline."
  (with-accessors ((data %lb-data)
                   (pos %lb-pos)
                   (len %lb-len)
                   (line-start %lb-line-start)
                   (seen-cr %lb-seen-cr))
      line-buffer
    (multiple-value-bind (string new-pos new-line-start new-seen-cr)
        (process-string-lines data :line-callback line-callback
                                   :start pos
                                   :end len
                                   :line-start line-start
                                   :seen-cr seen-cr
                                   :skip-trailing-line skip-trailing-line)
      (declare (ignore string))
      (setf pos new-pos
            line-start new-line-start
            seen-cr new-seen-cr)
      (lb-shift line-buffer)
      (unless skip-trailing-line
        (sb-delete line-buffer)
        (setf pos 0
              line-start 0
              seen-cr nil))
      line-buffer)))

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
