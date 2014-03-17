;;; COPYRIGHT 2006 Sergio Garcia <sergio.garcia@gmail.com>
;;; 
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 2.1 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA


(in-package #:gordon)

;;;;
;;;; Some utilities
;;;;


(defun bti (b)
  (if b 1 0))  

(defun itb (i)
  (not (equal i 0)))

(defun bif (test)
  (if test *yes-bit* *no-bit*))

;;; return the sum of the length of sequences in a list
(defun ttl (lst)
  (reduce 
   (lambda (x y)  (+ x (length y)))
   lst :initial-value 0))

(defun char-codes (str &optional (final nil))
  (if final
      (append (map 'list #'char-code str) '(0))
      (map 'list #'char-code str)))

;;; appends a list of elements including atoms if present

(defun xappend (&rest vals)
  (apply #'append (mapcar (lambda (x) (if (typep x 'list) x (list x))) vals)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
		 syms)
    ,@body))

;; generate a random number betwen min and max included
(defun xrandom (min max) 
  (+ min (random (1+ (- max min)))))

;; generate a vector of size size, with random numbers between min and max included
(defun random-vector (size min max)
  (let ((v (make-array size)))
    (dotimes (i size v)
      (setf (aref v i) (xrandom min max)))))

;;; some utilities
(defun skip (in n)
  (dotimes (i n)
    (read-byte in)))

(defun accumulate-n (n f) 
  (do 
   ((i 0 (1+ i)) 
    (acc 
     (list (funcall f))
     (cons (funcall f) acc)))
   ((= i (1- n)) acc)))
  
(defmacro debug-read-bytes (in &rest body)
  (with-gensyms (ip result)
    `(let ((,ip (file-position ,in))
	   (,result ,@body))
      (format t "Read bytes: ~A~%" (- (file-position in) ,ip))
      ,result)))

(defun filter-type (lst type) 
  (mapcan (lambda (x) (when (typep x type) (list x))) lst))

(defun find-if-case (lst type)
  (find-if (lambda (x) (typep x type)) lst))

(defun mapcar-pos (fun lst)
  (let ((counter -1))
    (mapcar (lambda (x) (funcall fun x (incf counter))) lst)))

(defun mapcar-with-last-result (fun lst first)
  (let ((tmp first))
    (mapcar (lambda (x) (setq tmp (funcall fun x tmp))) lst)))

(defun mapcar-with-last (fun lst first)
  (let ((tmp first))
    (mapcar (lambda (x) (let ((y (funcall fun x tmp))) (setq tmp x) y))  lst)))

(defun integer-to-214 (x)
  (if (= 0 (ldb (byte 1 15) x))
      (+ (ldb (byte 2 14) x) (coerce (/ (ldb (byte 14 0) x) 16384) 'float))
      (* -1 (+ (- 3 (ldb (byte 2 14) x)) (- 1 (coerce (/ (ldb (byte 14 0) x) 16384) 'float))))))

(defun integrize (x)
  (coerce x 'integer))

(defmacro incf-after (place &optional (delta 1))
  (with-gensyms (temp)
    `(let ((,temp ,place))
      (incf ,place ,delta)
      ,temp)))

(defmacro with (varname exp &body body)
    `(let ((,varname ,exp))
      (when ,varname ,@body)))

;;; Copyright Zach Beane
;;; No need to rely on external programs to find the dimensions of a
;;; JPEG; the format is specified in CCITT T.81 and is pretty easy to
;;; process for metadata.

(defun read-uint16 (stream)
  (logand #xFFFF
	  (logior (ash (read-byte stream)
		       8)
		  (ash (read-byte stream)
		       0))))


(defun standalone-marker-p (marker)
  ;; Table B.1
  (<= #xD0 marker #xD9))


(defun sof-marker-p (marker)
  ;; Table B.1
  (or (<= #xC0 marker #xC7)
      (<= #xC9 marker #xCB)
      (<= #xCD marker #xCF)))

(defun skip-length (stream)
  (let ((length (read-uint16 stream)))
    (file-position stream (+ (file-position stream)
			     (- length 2)))))

(defun sofn-dimensions (stream)
  ;; Section B.2.2
  (let ((length (read-uint16 stream))
	(precision (read-byte stream))
	(height (read-uint16 stream))
	(width (read-uint16 stream)))
    (declare (ignore length precision))
    (values width height)))


(defun jpeg-stream-dimensions (stream)
  "Returns the WIDTH and HEIGHT of the frame in the JPEG stream STREAM
as multiple values, or NIL if no frame is found."
  ;; Section B.1.1.2
  (do ((first-byte (read-byte stream nil)
		   next-byte)
       (next-byte (read-byte stream nil)
		  (read-byte stream nil)))
      ((not (and first-byte next-byte)))
    (when (and (= first-byte #xFF)
	       (/= next-byte #xFF #x00))
      (cond ((sof-marker-p next-byte)
	     (return (sofn-dimensions stream)))
	    ((not (standalone-marker-p next-byte))
	     (skip-length stream))))))


(defun jpeg-stream-p (stream)
  (and (eql (read-byte stream nil)
	    #xFF)
       (eql (read-byte stream nil)
	    #xD8)))

(defun jpeg-dimensions (file)
  "Returns the WIDTH and HEIGHT of the JPEG file FILE as multiple
values, or NIL if the file is not a valid JPEG file."
  (with-open-file (stream file
			  :direction :input
			  :element-type '(unsigned-byte 8))
    (when (jpeg-stream-p stream)
      (jpeg-stream-dimensions stream))))



;;; Elliott Johnson
;;; 03.27.2006
;;; CLOS utility functions heavily influenced by Peter Seibel.  Thanks Peter

(defun as-keyword (name) 
  "Borrowed directly from Peter Seibel's practical common lisp code."
  (intern (symbol-name name) :keyword))

(defun as-accessor (class-name slot-name &optional (string-separator "-"))
  "Created based upon Peter's AS-KEYWORD code.  This function will return 
an accessor function symbol based upon the concatenation of the class-name,
string-separator, and the slot name.  For example a slot BAR of class FOO
using the default string-separator will have a slot accessor of FOO-BAR."
  (intern
   (concatenate 'string
		(symbol-name class-name)
		string-separator
		(symbol-name slot-name))))

(defun describe-slot (class-name name &optional (initform nil) (type t) (allocation :instance))
  "Constructs a slot description list based upon the name, initform, and
type information."
  `(,name :accessor ,(as-accessor class-name name)
    :initform ,initform
    :initarg ,(as-keyword name)
    :type ,type
    :allocation ,allocation
    ))



(defun describe-slots (slot-descriptions class-name)
  "Applys the describe-slot function across all provided slot description
lists."
  (mapcar #'(lambda (x)(apply #'describe-slot (cons class-name (typecase x (cons x) (t (list x))))))
	  slot-descriptions))

(defmacro genclass (class-name
		    inheritance
		    slots-list 
		    &optional (docstring ""))
  "GENCLASS defines a class named CLASS-NAME which inherits from
the list INHERITANCE.  It's slots are defined by a list of slot
description lists of the form (slot-name initform type).  The initform
and type information are optional and default to NIL and T respectivly."
  `(defclass ,class-name ,inheritance
    ,(describe-slots slots-list class-name)
    (:documentation ,docstring)))
