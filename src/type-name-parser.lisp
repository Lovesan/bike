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
  (declare (type simple-character-string name)
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
           ;; .Net identifiers acutally allow '=' sign. But we utilize it
           ;;   for assembly property parsing.
           ;; Same goes for backquote - TODO: rework generic type parsing
           ;;   to allow backquotes inside names.
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

typeDefinition -> name namespaces typeDefinitionRest

namespaces -> DOT name namespaces / <empty>

typeDefinitionRest ->  BACKQUOTE INTEGER genericDefinitionRest
                     / NEST name typeDefinitionRest
                     / maybeHairyType

genericDefinitionRest -> NEST name genericDefinitionCont
                        / genericDefinitionEnd

genericDefinitionCont -> BACKQUOTE INTEGER genericDefinitionRest
                        / NEST name genericDefinitionCont
                        / genericDefinitionEnd

genericDefinitionEnd ->  OPEN maybeSpace genericDefinitionEndArgs maybeSpace maybeHairyType
                         / maybeHairyType

genericDefinitionEndArgs -> STAR maybeSpace arrayDimensionsRest CLOSE
                            / COMMA maybeSpace arrayDimensionsRest CLOSE
                            / CLOSE
                            / typeArgs maybeSpace CLOSE

typeArgs -> typeArg typeArgsRest

typeArgsRest -> COMMA maybeSpace typeArg typeArgsRest
               / <empty>

typeArg -> OPEN qualifiedTypeDefinition CLOSE
          / typeDefinition

maybeHairyType -> POINTER maybeHairyType
                  / REF
                  / OPEN maybeSpace arrayDimensions CLOSE maybeHairyType
                  / <empty>

;; 1) give up on ellipsis dimensions, since Type.GetType() doesn't accept it anyway
;; 2) .Net has this weird feature of arrays with variable lower bounds, which has its roots in
;;       backward compatibility with pre .NET Visual Basic COM types.
;;     Well, .NET actually has two types of arrays internally,
;;      the first one is SZArray and the other is MZArray.
;;     SZ arrays are roughly equivalent to one-dimensional simple-arrays from CL.
;;     MZ arrays, on the other hand, are 'hairy' and can be multi-dimensional
;;       and moreover, can have lower bounds different from 0.
;;     E.g. System.Int32[1...] is an array whose starting index is 1 instead of 0.
;;     The '*' as an array dimension, when printed by reflection API,
;;       designates that this particular one-dimensional array is an MZ array and
;;       therefore can have a lower bound greater than 0.
;;     Apparently, you can not 'syntactically' create an instance of an array
;;       that has bounds different from 0 either in C# or VB.NET
;;       apart from using reflection or one of the System.Array methods.
;;     Should we even consider this thing?

arrayDimensions -> STAR maybeSpace arrayDimensionsRest
                   / arrayDimensionsRest

arrayDimensionsRest -> COMMA maybeSpace arrayDimensions
                       / <empty>

maybeSpace -> SPACE / <empty>

maybeAssemblyDefinition -> COMMA maybeSpace assemblyName maybeSpace assemblyProps
                           / <empty>

assemblyName -> name assemblyNameRest

assemblyNameRest -> DOT name assemblyNameRest / <empty>

assemblyProps -> COMMA maybeSpace assemblyProp assemblyProps
                 / <empty>

assemblyProp -> IDENTIFIER("Version") maybeSpace EQ maybeSpace version maybeSpace
                / IDENTIFIER("Culture") maybeSpace EQ maybeSpace maybeIdentifier maybeSpace
                / IDENTIFIER("PublicKeyToken") maybeSpace EQ maybeSpace maybeIdentifier maybeSpace
                / IDENTIFIER("PublicKey") maybeSpace EQ maybeSpace maybeIdentifier maybeSpace
                / IDENTIFIER("Custom") maybeSpace EQ maybeSpace maybeIdentifier maybeSpace

version -> INTEGER DOT INTEGER DOT INTEGER DOT INTEGER / <empty>

maybeIdentifier -> IDENTIFIER / <empty>

name -> IDENTIFIER nameRest
        / EQ nameRest
        / INTEGER nameRest

nameRest -> IDENTIFIER nameRest
            / EQ nameRest
            / INTEGER nameRest
            / <empty>
|#

(defun parse-type-name (name)
  (declare (type simple-character-string name))
  (let ((tokenizer (make-type-name-tokenizer name))
        (nextp nil)
        (token :eof)
        (start 0)
        (end 0))
    (labels ((err () (error 'type-name-unexpected-token-error
                            :datum name
                            :token token
                            :value (unless (eq token :eof) (subseq name start end))
                            :position start))
             (peek ()
               (unless nextp
                 (multiple-value-setq (token start end) (funcall tokenizer))
                 (setf nextp t)))
             (next () (setf nextp nil) (peek))
             (expect (what) (unless (eq what token) (err)))
             (value= (what) (string-equal what name :start2 start :end2 end))
             (consume (what) (expect what) (next))
             (name-rest (name-start)
               (peek)
               (case token
                 ((:identifier :eq :integer)
                  (next)
                  (name-rest name-start))))
             (name (&aux (name-start 0))
               (peek)
               (setf name-start start)
               (case token
                 ((:identifier :eq :integer)
                  (next)
                  (name-rest name-start))
                 (t (err))))
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
                 (name)
                 (assembly-name-rest)))
             (assembly-name ()
               (peek)
               (name)
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
             (array-dim-rest ()
               (peek)
               (cond ((eq token :comma)
                      (next)
                      (maybe-space)
                      (array-dims))
                     (t (values 0 nil))))
             (array-dims ()
               (peek)
               (cond ((eq token :star)
                      (next)
                      (maybe-space)
                      (values (1+ (array-dim-rest)) t))
                     (t (multiple-value-bind (n starp) (array-dim-rest)
                          (values (1+ n) starp)))))
             (maybe-hairy-type (type &aux rank starp)
               (peek)
               (case token
                 (:star (next) (maybe-hairy-type (list '* type)))
                 (:ref (next) (list :ref type))
                 (:open
                  (next)
                  (maybe-space)
                  (setf (values rank starp) (array-dims))
                  (consume :close)
                  (maybe-hairy-type (list :array type (if (and starp (= rank 1))
                                                        '*
                                                        rank))))
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
             (gen-def-end-args (def-start def-end argc
                                &aux (rank 0)
                                     (args '())
                                     (type (subseq name def-start def-end)))
               (peek)
               (case token
                 (:star
                  (next)
                  (maybe-space)
                  (setf rank (1+ (array-dim-rest)))
                  (consume :close)
                  (list :array type (if (= rank 1) '* rank)))
                 (:comma
                  (next)
                  (maybe-space)
                  (setf rank (+ 2 (array-dim-rest)))
                  (consume :close)
                  (list :array type rank))
                 (:close
                  (next)
                  (list :array type 1))
                 (t
                  (setf args (type-args))
                  (maybe-space)
                  (consume :close)
                  (unless (= argc (length args))
                    (error 'generic-argument-count-mismatch
                           :token type
                           :value (subseq name def-end start)
                           :position def-end
                           :datum name))
                  (cons type args))))
             (gen-def-end (def-start argc &aux type (def-end 0))
               (peek)
               (setf def-end start)
               (case token
                 (:open
                  (next)
                  (maybe-space)
                  (setf type (gen-def-end-args def-start def-end argc))
                  (maybe-space)
                  (maybe-hairy-type type))
                 (t (maybe-hairy-type (subseq name def-start def-end)))))
             (gen-def-cont (def-start argc &aux (s 0) (e 0))
               (peek)
               (case token
                 (:backquote
                  (next)
                  (setf s start)
                  (consume :integer)
                  (setf e start)
                  (gen-def-rest def-start (+ argc (parse-integer name :start s :end e))))
                 (:nest
                  (next)
                  (name)
                  (gen-def-cont def-start argc))
                 (t (gen-def-end def-start argc))))
             (gen-def-rest (def-start argc)
               (peek)
               (case token
                 (:nest
                  (next)
                  (name)
                  (gen-def-cont def-start argc))
                 (t (gen-def-end def-start argc))))
             (type-def-rest (def-start &aux (s 0) (e 0))
               (peek)
               (case token
                 (:backquote
                  (next)
                  (setf s start)
                  (consume :integer)
                  (setf e start)
                  (gen-def-rest def-start (parse-integer name :start s :end e)))
                 (:nest
                  (next)
                  (name)
                  (type-def-rest def-start))
                 (t (maybe-hairy-type (subseq name def-start start)))))
             (namespaces ()
               (peek)
               (when (eq token :dot)
                 (next)
                 (name)
                 (namespaces)))
             (type-def ()
               (peek)
               (let ((def-start start))
                 (name)
                 (namespaces)
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
