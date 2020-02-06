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

(in-package #:bike)

(defun shutdown-bike ()
  (format *error-output* "~&[bike] Clearing invocation cache~%")
  (clear-invocation-cache)
  (format *error-output* "~&[bike] Wiping out lisp handles~%")
  (%clear-handle-table)
  (format *error-output* "~&[bike] Performing full GC~%")
  (tg:gc :full t)
  (format *error-output* "~&[bike] Performing CoreCLR GC~%")
  (gc-collect)
  (gc-wait-for-pending-finalizers)
  (format *error-output* "~&[bike] Shutting down CoreCLR~%")
  (shutdown-coreclr)
  #+(and sbcl coreclr-windows)
  (progn
    ;; TODO: Figure out why sbcl crashes without this
    (format *error-output* "~&[bike] Waiting 5 sec. for CoreCLR to shut down~%")
    (tg:gc :full t)
    (sleep 5))
  (format *error-output* "~&[bike] Shutdown complete~%"))

(uiop:register-image-dump-hook 'shutdown-bike)

;;; vim: ft=lisp et
