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

(defun make-type-name-tokenizer (name &key (start 0) end)
  (declare (type dotnet-name name)
           (type non-negative-fixnum start))
  "Returns a lambda which tokenizes type name string"
  (let* ((end (or end (length name)))
         (i 0)
         (c nil))
    (declare (type non-negative-fixnum i end)
             (type (or null character) c))
    (flet ((peek () (cond ((< i end) (setf c (char name i)))
                          (t (setf c nil))))
           (next () (when (< i end) (incf i)))
           (eofp () (null c))
           (spacep () (case c ((#\space #\tab #\return #\newline) t)))
           (specialp () (case c ((#\, #\+ #\[ #\] #\* #\& #\. #\= #\`) t)))
           (digitp () (and c (digit-char-p c 10)))
           (start-here () (setf start i)))
      (lambda ()
        (macrolet ((token (name) `(return (values ,name start i))))
          (prog ()
           :start (peek)
             (cond ((eofp) (start-here) (token :eof))
                   ((spacep) (start-here) (next) (go :space))
                   ((eql c #\,) (start-here) (next) (token :comma))
                   ((eql c #\+) (start-here) (next) (token :nest))
                   ((eql c #\[) (start-here) (next) (token :open))
                   ((eql c #\]) (start-here) (next) (token :close))
                   ((eql c #\*) (start-here) (next) (token :star))
                   ((eql c #\&) (start-here) (next) (token :ref))
                   ((eql c #\`) (start-here) (next) (token :backquote))
                   ((eql c #\.) (start-here) (next) (go :dot))
                   ((eql c #\=) (start-here) (next) (token :eq))
                   ((eql c #\\) (start-here) (next) (go :escape))
                   ((digitp) (start-here) (next) (go :integer))
                   (t (start-here) (next) (go :identifier)))
           :escape (peek)
             (cond ((eofp) (go :eof-error))
                   (t (next) (go :identifier)))
           :dot (peek)
             (cond ((eql c #\.) (next) (go :dot2))
                   (t (token :dot)))
           :dot2 (peek)
             (cond ((eql c #\.) (next) (go :dot3))
                   (t (token :between)))
           :dot3 (peek)
             (cond ((eql c #\.) (go :error))
                   (t (token :ellipsis)))
           :identifier (peek)
             (cond ((or (eofp) (spacep) (specialp))
                    (token :identifier))
                   ((eql c #\\) (next) (go :escape))
                   (t (next) (go :identifier)))
           :integer (peek)
             (cond ((digitp) (next) (go :integer))
                   ((or (eofp) (spacep) (specialp)) (token :integer))
                   (t (next) (go :identifier)))
           :space (peek)
             (cond ((spacep) (next) (go :space))
                   (t (token :space)))
           :eof-error
             (error 'type-name-parser-eof
                    :datume name
                    :position i)
           :error
             (error 'type-name-parser-error
                    :datum name
                    :position i
                    :character c)))))))

#|

qualifiedTypeDefinition -> maybeSpace typeDefinition maybeSpace maybeAssemblyDefinition EOF

typeDefinition -> IDENTIFIER namespaces nestedTypes typeDefinitionRest

namespaces -> DOT IDENTIFIER namespaces / <empty>

nestedTypes -> NEST IDENTIFIER nestedTypes
              / <empty>

typeDefinitionRest -> BACKQUOTE INTEGER maybeTypeArgs maybeHairyType
                     / maybeHairyType

maybeTypeArgs -> OPEN maybeSpace typeArgs maybeSpace CLOSE
                 / <empty>

typeArgs -> typeArg typeArgsRest

typeArgsRest -> COMMA maybeSpace typeArg typeArgsRest
               / <empty>

typeArg -> OPEN qualifiedTypeDefinition CLOSE
          / typeDefinition

maybeHairtyType -> POINTER maybeHairyType
                  / REF
                  / OPEN arrayDimensions CLOSE maybeHairyType
                  / <empty>

;; give up on ellipsis dimensions, since Type.GetType() doesn't accept it anyway

arrayDimensions -> maybeSpace arrayDimension maybeSpace arrayDimensionsRest

arrayDimensionsRest -> COMMA arrayDimension maybeSpace arrayDimensionsRest
                     / <empty>

arrayDimension -> STAR / <empty>

maybeSpace -> SPACE / <empty>

maybeAssemblyDefinition -> COMMA maybeSpace assemblyName maybeSpace assemblyProps
                           / <empty>

assemblyName -> IDENTIFIER assemblyNameRest

assemblyNameRest -> DOT IDENTIFIER assemblyNameRest / <empty>

assemblyProps -> COMMA maybeSpace assemblyProp assemblyProps
                 / <empty>

assemblyProp -> IDENTIFIER("Version") maybeSpace EQ maybeSpace version maybeSpace
                / IDENTIFIER("Culture") maybeSpace EQ maybeSpace maybeIdentifier maybeSpace
                / IDENTIFIER("PublicKeyToken") maybeSpace EQ maybeSpace maybeIdentifier maybeSpace
                / IDENTIFIER("PublicKey") maybeSpace EQ maybeSpace maybeIdentifier maybeSpace
                / IDENTIFIER("Custom") maybeSpace EQ maybeSpace maybeIdentifier maybeSpace

version -> INTEGER DOT INTEGER DOT INTEGER DOT INTEGER / <empty>

maybeIdentifier -> IDENTIFIER / <empty>

|#

(defun parse-type-name (name)
  (declare (type dotnet-name name))
  (let ((tokenizer (make-type-name-tokenizer name))
        (nextp nil)
        (token :eof)
        (start 0)
        (end 0))
    (labels ((err () (error 'type-name-unexpected-token-error
                            :datum name
                            :token token
                            :value (subseq name start end)
                            :position start))
             (peek ()
               (unless nextp
                 (multiple-value-setq (token start end) (funcall tokenizer))
                 (setf nextp t)))
             (next () (setf nextp nil) (peek))
             (expect (what) (unless (eq what token) (err)))
             (value= (what) (string-equal what name :start2 start :end2 end))
             (consume (what) (expect what) (next))
             (maybe-identifier ()
               (peek)
               (case token
                 (:identifier (next))))
             (maybe-space ()
               (peek)
               (case token
                 (:space (next))))
             (maybe-version ()
               (peek)
               (when (eq token :integer)
                 (next)
                 (consume :dot)
                 (consume :integer)
                 (consume :dot)
                 (consume :integer)
                 (consume :dot)
                 (consume :integer)))
             (assembly-prop ()
               (peek)
               (expect :identifier)
               (cond ((value= "Version")
                      (next)
                      (maybe-space)
                      (consume :eq)
                      (maybe-space)
                      (maybe-version)
                      (maybe-space))
                     ((value= "Culture")
                      (next)
                      (maybe-space)
                      (consume :eq)
                      (maybe-space)
                      (maybe-identifier)
                      (maybe-space))
                     ((value= "PublicKeyToken")
                      (next)
                      (maybe-space)
                      (consume :eq)
                      (maybe-space)
                      (maybe-identifier)
                      (maybe-space))
                     ((value= "PublicKey")
                      (next)
                      (maybe-space)
                      (consume :eq)
                      (maybe-space)
                      (maybe-identifier)
                      (maybe-space))
                     ((value= "Custom")
                      (next)
                      (maybe-space)
                      (consume :eq)
                      (maybe-space)
                      (maybe-identifier)
                      (maybe-space))
                     (t (err))))
             (assembly-props ()
               (peek)
               (when (eq token :comma)
                 (next)
                 (maybe-space)
                 (assembly-prop)
                 (assembly-props)))
             (assembly-name-rest ()
               (peek)
               (when (eq token :dot)
                 (next)
                 (consume :identifier)
                 (assembly-name-rest)))
             (assembly-name ()
               (peek)
               (consume :identifier)
               (assembly-name-rest))
             (maybe-assembly-definition (&aux (s 0) (e 0))
               (peek)
               (when (eq token :comma)
                 (next)
                 (maybe-space)
                 (setf s start)
                 (assembly-name)
                 (maybe-space)
                 (assembly-props)
                 (setf e start)
                 (subseq name s e)))
             (array-dim ()
               (peek)
               (when (eq token :star)
                 (next)))
             (array-dim-rest ()
               (peek)
               (cond ((eq token :comma)
                      (next)
                      (array-dim)
                      (maybe-space)
                      (1+ (array-dim-rest)))
                     (t 0)))
             (array-dims ()
               (maybe-space)
               (array-dim)
               (maybe-space)
               (1+ (array-dim-rest)))
             (maybe-hairy-type (type &aux rank)
               (peek)
               (case token
                 (:star (next) (maybe-hairy-type (list '* type)))
                 (:ref (next) (list :ref type))
                 (:open
                  (next)
                  (setf rank (array-dims))
                  (consume :close)
                  (maybe-hairy-type (list :array type rank)))
                 (t type)))
             (type-arg ()
               (peek)
               (cond ((eq token :open)
                      (next)
                      (prog1 (qualified-type-def)
                        (consume :close)))
                     (t (type-def))))
             (type-args-rest ()
               (peek)
               (when (eq token :comma)
                 (next)
                 (maybe-space)
                 (cons (type-arg) (type-args-rest))))
             (type-args ()
               (cons (type-arg) (type-args-rest)))
             (maybe-type-args ()
               (peek)
               (when (eq token :open)
                 (next)
                 (maybe-space)
                 (prog1 (type-args)
                   (maybe-space)
                   (consume :close))))
             (type-def-rest (def-start &aux (s 0) (e 0) (n 0) args)
               (peek)
               (cond ((eq token :backquote)
                      (next)
                      (setf s start)
                      (consume :integer)
                      (setf e start)
                      (setf n (parse-integer name :start s :end e))
                      (setf args (maybe-type-args))
                      (when (and args (/= (length args) n))
                        (error 'generic-argument-count-mismatch
                               :datum name
                               :token (subseq name def-start e)
                               :position e
                               :value (subseq name e start)))
                      (maybe-hairy-type
                       (let ((def-name (subseq name def-start e)))
                         (if args
                           (cons def-name args)
                           def-name))))
                     (t (maybe-hairy-type (subseq name def-start start)))))
             (nested-types ()
               (peek)
               (when (eq token :nest)
                 (next)
                 (consume :identifier)
                 (nested-types)))
             (namespaces ()
               (peek)
               (when (eq token :dot)
                 (next)
                 (consume :identifier)
                 (namespaces)))
             (type-def ()
               (peek)
               (let ((def-start start))
                 (consume :identifier)
                 (namespaces)
                 (nested-types)
                 (type-def-rest def-start)))
             (qualified-type-def ()
               (maybe-space)
               (let ((def (type-def)))
                 (maybe-space)
                 (let ((asm (maybe-assembly-definition)))
                   (peek)
                   (if asm
                     (list :qualified def asm)
                     def))))
             (start ()
               (prog1 (qualified-type-def)
                 (peek)
                 (consume :eof))))
      (start))))

;;; vim: ft=lisp et
