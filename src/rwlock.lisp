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

;; Read preferrence. Reentrant. Upgradable.

(defstruct (rwlock-thread-info (:constructor make-rwlti)
                               (:conc-name rwlti-)
                               (:copier nil))
  (thread nil :type (or null bt2:thread))
  (read-count 0 :type non-negative-fixnum)
  (write-count 0 :type non-negative-fixnum)
  (upgrade-count 0 :type non-negative-fixnum)
  (next nil :type (or null rwlock-thread-info)))

(defstruct (rwlock (:constructor %make-rwlock (name))
                   (:predicate rwlockp)
                   (:copier nil)
                   (:conc-name %rwlock-))
  "Read-Write lock with read preferrence"
  (name nil :type (or null string))
  (cv-lock (bt2:make-lock) :type bt2:lock :read-only t)
  (reader-cv (bt2:make-condition-variable)
   :type bt2:condition-variable :read-only t)
  (writer-cv (bt2:make-condition-variable)
   :type bt2:condition-variable :read-only t)
  (upgrade-cv (bt2:make-condition-variable)
   :type bt2:condition-variable :read-only t)
  (reader-count 0 :type fixnum)
  (writer-count 0 :type fixnum)
  (upgrading-count 0 :type fixnum)
  (waiting-reader-count 0 :type fixnum)
  (waiting-writer-count 0 :type fixnum)
  (waiting-upgrade-count 0 :type fixnum)
  ;; these are reused
  (threads nil :type (or null rwlock-thread-info))
  (writer nil :type (or null bt2:thread)))

(defun make-rwlock (&key name)
  (declare (type (or null string) name))
  "Creates an instance of Read-Write lock with read preferrence"
  (%make-rwlock name))

(defun rwlock-name (rwlock)
  (declare (type rwlock rwlock))
  "Retrieves name of a read-write lock"
  (%rwlock-name rwlock))

(defun rwlock-thread-info (rwlock thread &optional alloc)
  (declare (type rwlock rwlock)
           (type bt2:thread thread))
  (with-accessors ((threads %rwlock-threads))
      rwlock
    (let ((info (loop :for ti = threads :then (rwlti-next ti)
                      :while ti
                      :when (eq thread (rwlti-thread ti))
                        :return ti)))
      (when (and (null info) alloc)
        (let ((free-info (loop :for ti = threads :then (rwlti-next ti)
                               :while ti
                               :when (null (rwlti-thread ti))
                                 :return ti)))
          (if free-info
            (setf (rwlti-thread free-info) thread
                  info free-info)
            (setf info (make-rwlti :thread thread
                                   :next threads)
                  threads info))))
      info)))

(defun rwlock-modify-count (rwlock thread &key (reader-delta 0)
                                               (writer-delta 0)
                                               (upgrade-delta 0))
  (declare (type rwlock rwlock)
           (type fixnum reader-delta writer-delta upgrade-delta))
  (let ((info (rwlock-thread-info rwlock thread t)))
    (macrolet ((modify (rwlock-accessor info-accessor delta error-name)
                 `(unless (zerop ,delta)
                    (let ((new-global-count (+ (,rwlock-accessor rwlock) ,delta))
                          (new-thread-count (+ (,info-accessor info) ,delta)))
                      (when (minusp new-global-count)
                        (error "Negative ~a count in rwlock ~s"
                               ,error-name rwlock))
                      (when (minusp new-thread-count)
                        (error "Negative thread ~s ~a count in rwlock ~s"
                               thread ,error-name rwlock))
                      (setf (,rwlock-accessor rwlock) new-global-count
                            (,info-accessor info) new-thread-count)))))
      (modify %rwlock-reader-count rwlti-read-count reader-delta "read")
      (modify %rwlock-writer-count rwlti-write-count writer-delta "write")
      (modify %rwlock-upgrading-count rwlti-upgrade-count upgrade-delta "upgrade")
      (when (and (zerop (rwlti-read-count info))
                 (zerop (rwlti-write-count info))
                 (zerop (rwlti-upgrade-count info)))
        (setf (rwlti-thread info) nil))
      info)))

(defun rwlock-wait (rwlock wait-type)
  (declare (type rwlock rwlock)
           (type (member :read :write :upgrade) wait-type))
  (with-accessors ((waiting-reader-count %rwlock-waiting-reader-count)
                   (waiting-writer-count %rwlock-waiting-writer-count)
                   (waiting-upgrade-count %rwlock-waiting-upgrade-count)
                   (lock %rwlock-cv-lock)
                   (reader-cv %rwlock-reader-cv)
                   (writer-cv %rwlock-writer-cv)
                   (upgrade-cv %rwlock-upgrade-cv))
      rwlock
    (ecase wait-type
      (:read
       (incf waiting-reader-count)
       (unwind-protect
            (bt2:condition-wait reader-cv lock)
         (decf waiting-reader-count)))
      (:write
       (incf waiting-writer-count)
       (unwind-protect
            (bt2:condition-wait writer-cv lock)
         (decf waiting-writer-count)))
      (:upgrade
       (incf waiting-upgrade-count)
       (unwind-protect
            (bt2:condition-wait upgrade-cv lock)
         (decf waiting-upgrade-count))))
    (values)))

(defun rwlock-begin-read (rwlock)
  (declare (type rwlock rwlock))
  (with-accessors ((lock %rwlock-cv-lock)
                   (reader-count %rwlock-reader-count)
                   (writer-count %rwlock-writer-count)
                   (writer %rwlock-writer))
      rwlock
    (let ((self (bt2:current-thread)))
      (bt2:with-lock-held (lock)
        (loop
          (cond ((or (plusp reader-count)
                     (eq writer self))
                 (rwlock-modify-count rwlock self :reader-delta 1)
                 (return))
                ((zerop writer-count)
                 (unless (null writer)
                   (error "Dangling writer in rwlock ~s" rwlock))
                 (rwlock-modify-count rwlock self :reader-delta 1)
                 (return))
                (t (rwlock-wait rwlock :read)))))))
  (values))

(defun rwlock-end-read (rwlock)
  (declare (type rwlock rwlock))
  (with-accessors ((lock %rwlock-cv-lock)
                   (writer-cv %rwlock-writer-cv)
                   (upgrade-cv %rwlock-upgrade-cv)
                   (reader-count %rwlock-reader-count)
                   (waiting-writer-count %rwlock-waiting-writer-count)
                   (waiting-upgrade-count %rwlock-waiting-upgrade-count)
                   (writer %rwlock-writer))
      rwlock
    (let ((self (bt2:current-thread))
          wake-writer wake-upgrading)
      (bt2:with-lock-held (lock)
        (rwlock-modify-count rwlock self :reader-delta -1)
        (when (zerop reader-count)
          ;; prefer waking up upgrading readers
          (cond ((plusp waiting-upgrade-count)
                 (setf wake-upgrading t))
                ((plusp waiting-writer-count)
                 (setf wake-writer t)))))
      (when wake-writer (bt2:condition-notify writer-cv))
      (when wake-upgrading (bt2:condition-notify upgrade-cv))))
  (values))

(defun rwlock-begin-write (rwlock)
  (declare (type rwlock rwlock))
  (with-accessors ((lock %rwlock-cv-lock)
                   (reader-count %rwlock-reader-count)
                   (writer-count %rwlock-writer-count)
                   (writer %rwlock-writer))
      rwlock
    (let ((self (bt2:current-thread)))
      (bt2:with-lock-held (lock)
        (loop (when (eql writer self)
                (rwlock-modify-count rwlock self :writer-delta 1)
                (return))
              (cond ((zerop writer-count)
                     (unless (null writer)
                       (error "Dangling writer in rwlock ~s" rwlock))
                     (let* ((info (rwlock-thread-info rwlock self t))
                            (self-rc (rwlti-read-count info))
                            (other-rc (- reader-count self-rc))
                            (self-uc (rwlti-upgrade-count info)))
                       (cond ((zerop other-rc)
                              (rwlock-modify-count rwlock self :writer-delta 1
                                                               :upgrade-delta (- self-uc)
                                                               :reader-delta self-uc)
                              (setf writer self)
                              (return))
                             ((and (plusp self-rc) (zerop self-uc))
                              (rwlock-modify-count rwlock self :reader-delta (- self-rc)
                                                               :upgrade-delta self-rc)
                              (rwlock-wait rwlock :upgrade))
                             ((plusp self-uc) ;; Woken by accident?
                              (rwlock-wait rwlock :upgrade))
                             (t (rwlock-wait rwlock :write)))))
                    (t (let* ((info (rwlock-thread-info rwlock self))
                              (self-uc (or (and info (rwlti-upgrade-count info))
                                           0)))
                         (if (plusp self-uc)
                           (rwlock-wait rwlock :upgrade)
                           (rwlock-wait rwlock :write)))))))))
  (values))

(defun rwlock-end-write (rwlock)
  (declare (type rwlock rwlock))
  (with-accessors ((lock %rwlock-cv-lock)
                   (reader-cv %rwlock-reader-cv)
                   (writer-cv %rwlock-writer-cv)
                   (upgrade-cv %rwlock-upgrade-cv)
                   (writer-count %rwlock-writer-count)
                   (waiting-writer-count %rwlock-waiting-writer-count)
                   (waiting-reader-count %rwlock-waiting-reader-count)
                   (waiting-upgrade-count %rwlock-waiting-upgrade-count)
                   (writer %rwlock-writer))
      rwlock
    (let ((self (bt2:current-thread))
          wake-writer wake-readers wake-upgrading)
      (bt2:with-lock-held (lock)
        (unless (eq writer self)
          (error "~s does not own write lock ~s" self rwlock))
        (rwlock-modify-count rwlock self :writer-delta -1)
        (when (zerop writer-count)
          (setf writer nil)
          ;; prefer waking up readers
          (cond ((plusp waiting-reader-count)
                 (setf wake-readers t))
                ((plusp waiting-upgrade-count)
                 (setf wake-upgrading t))
                ((plusp waiting-writer-count)
                 (setf wake-writer t)))))
      (when wake-readers
        (bt2:condition-broadcast reader-cv))
      (when wake-upgrading
        (bt2:condition-notify upgrade-cv))
      (when wake-writer
        (bt2:condition-notify writer-cv))))
  (values))

(defmacro with-read-lock ((rwlock) &body body)
  (with-gensyms (lock)
    `(let ((,lock ,rwlock))
       (declare (type rwlock ,lock))
       (rwlock-begin-read ,rwlock)
       (unwind-protect (locally ,@body)
         (rwlock-end-read ,lock)))))

(defmacro with-write-lock ((rwlock) &body body)
  (with-gensyms (lock)
    `(let ((,lock ,rwlock))
       (declare (type rwlock ,lock))
       (rwlock-begin-write ,lock)
       (unwind-protect (locally ,@body)
         (rwlock-end-write ,lock)))))

(defun ensure-bt2-classes-finalized ()
  (c2mop:ensure-finalized (find-class 'bt2:thread))
  (c2mop:ensure-finalized (find-class 'bt2:lock))
  (values))

(register-image-restore-hook 'ensure-bt2-classes-finalized t)

;;; vim: ft=lisp et
