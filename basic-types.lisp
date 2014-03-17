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
;;;;
;;;; SWF Basic Types library
;;;; 
;;;;

;;; convertion to SWF integer types as byte arrays
;; return the byte corresponding to the 8 bits
;; of a bit array that start in x


(deftype byte-array ()
  `(vector (unsigned-byte 8)))



(deftype bit-array ()
  `(vector bit))


(defun i2SI8 (val)
  (is2lesbitaba `(,val) `(,8)))

(defun i2UI8 (val)
  (is2leubitaba `(,val) `(,8)))

(defun i2SI16 (val)
  (is2lesbitaba `(,val) `(,16)))

(defun i2UI16 (val)
  (is2lesbitaba `(,val) `(,16)))

(defun i2SI32 (val)
  (is2lesbitaba `(,val) `(,32)))

(defun i2UI32 (val)
  (is2lesbitaba `(,val) `(,32)))

(defun f2f (val)
  (multiple-value-bind (high low) (floor (abs val))
    (let ((s (if (>= val 0) 1 -1)))
      (concba 
       (i2UI16 (floor (* low (expt 2 16))))
       (i2SI16 (* s high))))))

(defmacro bita2byte (bita x)
  `(coerce (+ ,@(mapcar (lambda (n) `(* (bit ,bita (+ ,x ,n)) ,(expt 2 (- 7 n)))) '(0 1 2 3 4 5 6 7 ))) '(unsigned-byte 8)))

;;; returns a array of bytes with the 
;;; concatenation of arrays of bits containint
;;; the  representations of a list of numbers
;;; (signed or unsigned according to signs)
;;; in the respective sizes. 
;;; endianism applies to the order of the bytes in 
;;; the array.

(defun vs2bita (vals sizes signs)
  (apply #'concbita 
	 (mapcar 
	  (lambda (val siz sig) 
	    (funcall
	     (if (integerp val)
		 (if sig 
		     #'i2sbita
		     #'i2ubita)
		 #'f2sbita)
	     val siz))
	  vals sizes signs)))

;;; Same as the former, but it returns the array of bits
;;; transformed to an array of bytes, with little endianism and padding
;;; if necessary

(defun vs2bitaba (vals sizes signs &optional (endianism :BE))
  (funcall (if (eq endianism :LE) 
	       #'bita2leba 
	       #'bita2beba)
	   (vs2bita vals sizes signs)
	   ))

(defun is2leubitaba (ints sizes)
  (bita2leba (apply #'concbita (mapcar #'i2ubita ints sizes))))

(defun is2lesbitaba (ints sizes)
  (bita2leba (apply #'concbita (mapcar #'i2sbita ints sizes))))

;;; like the former, but big endian
(defun is2beubitaba (ints sizes)
  (bita2beba (apply #'concbita (mapcar #'i2ubita ints sizes))))

(defun is2besbitaba (ints sizes)
  (bita2beba (apply #'concbita (mapcar #'i2sbita ints sizes))))

;;; return a bit array, big endian, containing the signed representation
;;; of a list of ints, prefixed with an array of bitsmaxsize with the
;;; unsigned integer representation of the number of bits used for all
;;; the other values. 

(defun is2besbitawithmax (vals bitsmaxsize)
  (let ((maxsize (apply #'max (mapcar (lambda (x) (needsize x t)) vals))))
    (concbita (i2ubita maxsize bitsmaxsize)
	      (apply #'concbita (mapcar 
				 #'i2sbita vals 
				 (make-list (length vals) :initial-element maxsize))))))

(defun is2beubitawithmax (vals bitsmaxsize)
  (let ((maxsize (apply #'max (mapcar (lambda (x) (needsize x nil)) vals))))
    (concbita (i2ubita maxsize bitsmaxsize)
	      (apply #'concbita (mapcar 
				 #'i2ubita vals 
				 (make-list (length vals) :initial-element maxsize))))))
 

(defun fs2besbitawithmax (vals bitsmaxsize)
  (let ((maxsize (apply #'max (mapcar (lambda (x) (needsize x t)) vals))))
    (concbita (i2ubita maxsize bitsmaxsize)
	      (apply #'concbita (mapcar 
				 #'f2sbita vals 
				 (make-list (length vals) :initial-element maxsize))))))

;;; return a bit array of size b with the unsigned representation
;;; of integer c

(defun i2ubita (c b)
  (let ((rarray (make-array b :element-type 'bit :initial-element 0)))
      (do* ((i (1- b) (1- i)) 
	   (n c (ash n -1))
	   (v (bti (logbitp 0 c)) (bti (logbitp 0 n))))
	  ((= i -1) rarray)
	(setf (bit rarray i) v))))


;;; Return a bit array of size b witht he signed representation of integer c

(defun f2sbita (c b)
    (check-type c float)
      (multiple-value-bind (high low) (floor c)
	    (concbita
	             (i2sbita high (- b 16))
		            (i2ubita (floor (* low (expt 2 16))) 16)
			           )))


;;; return a bit array of size b with the signed representation
;;; of integer c

(defun i2sbita (c b)
  (if (>= c 0)
      (i2ubita c b)
      (bit-not (i2ubita (1- (abs c)) b))))


(defun concbita (&rest others)
  (apply #'concatenate '(vector bit) others))

(defun concba (&rest others)
  (apply #'concatenate '(vector (unsigned-byte 8)) others))


;;; inyect the contents of the first array to the second 
;;; array, with an optional argument on the start point
(defun inject (sources dest &optional (sp 0))
  (unless (null sources) 
    (let ((source (car sources)))
      (dotimes (i (array-total-size source) dest)
	(setf (aref dest (+ i sp)) (aref source i)))
      (inject (cdr sources) dest (+ sp (length source)))
      dest)))



;;; pad a bit array so it ends in a multiple of 8
(defun pad8 (bita)
  (let ((osize (array-total-size bita)))
    (if (zerop (mod osize 8))
	bita
	(adjust-array bita (+ osize (- 8 (mod osize 8))) :initial-element 0))))

;;; String to byte array
(defun string2string2 (s &optional (with-end nil))
  (let ((result-length (+ (length s) (if with-end 1 0))))
    (vs2bitaba (char-codes s with-end) 
	       (make-list result-length :initial-element 8)
	       (make-list result-length :initial-element nil))))



(defun string2string (s &optional (with-end nil))
  (let ((values (loop for code in (char-codes s with-end) appending (if (< code 128) (list code)
							       (list (+ 192 (floor (/ code 64))) (+ 128 (mod code 64)))))))
    (let ((result-length (+ (length values) (if with-end 1 0))))
      (vs2bitaba values 
		 (make-list result-length :initial-element 8)
		 (make-list result-length :initial-element nil)))))











;;; returns a big endian array of bytes with the 
;;; concatenation of arrays of bits containint
;;; the unsigned representations of a list of integers
;;; in the respective sizes

;;; re-implement by increasing array size?

(defun bita2beba (bita)
  (let* ((pbita (pad8 bita))
	 (bitsize (array-total-size pbita))
	 (barray (make-array (/ bitsize 8) 
			     :element-type '(unsigned-byte 8))))
    (dotimes (i (array-total-size barray) barray)
      (setf (aref barray i) (bita2byte pbita (* 8 i))))))

(defun bita2leba (bita)
  (let* ((pbita (pad8 bita))
	 (bitsize (array-total-size pbita))
	 (barray (make-array (/ bitsize 8) 
			     :element-type '(unsigned-byte 8)))
	 (basize (array-total-size barray)))
    (dotimes (i basize barray)
      (setf (aref barray (- basize (1+ i))) (bita2byte pbita (* 8 i))))))


(defun needsize (i s)
  "return the number of bits necessary to represent an integer. s
   set to true means that the repsentation is signed"
  (typecase i 
    (integer (+ (integer-length i) (bti s)))
    (float (let* ((vs (> i 0))
		  (hp (* (if vs 1 -1) (floor (abs i)))))
	     (+ 16 (needsize hp s))))))



