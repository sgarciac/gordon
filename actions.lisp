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

;; the action

(defparameter *action-record-data-marshallers* (make-hash-table))
(defparameter *action-record-codes* (make-hash-table))

(defun action-record-type (action-record)
  (gethash (type-of action-record) *action-record-codes*))

(defun marshall-action-record-data (action-record)
  (funcall (gethash (type-of action-record) *action-record-data-marshallers*) action-record))

(defmacro add-action-record-type(name code fields marshaller)
  `(progn (defstruct ,name ,@fields)
	 (setf (gethash ',name *action-record-codes*) ,code)
	 (setf (gethash ',name *action-record-data-marshallers*) ,marshaller)
	 ))

;;; Marshall a tag
(defun marshall-action-record (action-record)
  (let* ((mard (flat-marshall-data (marshall-action-record-data action-record)))
	 (art (action-record-type action-record))
	 
	 (l (length mard))
	 (longp (>= art 128))
	 
	 (recordheader 
	  (if longp
	      (list 
	       (i2ui8 art)
	       (i2ui16 l))
	      (i2ui8 art))))
	 (xappend recordheader mard)))

(add-action-record-type action-record-next-frame 
			4
			()
			(lambda (x) nil))

(add-action-record-type action-record-previous-frame 
			5
			()
			(lambda (x) nil))

(add-action-record-type action-record-play
			6
			()
			(lambda (x) nil))

(add-action-record-type action-record-stop
			7
			()
			(lambda (x) nil))

(add-action-record-type action-record-toggle-quality
			8
			()
			(lambda (x) nil))

(add-action-record-type action-record-stop-sounds
			9
			()
			(lambda (x) nil))



(add-action-record-type action-record-goto-frame
			129
			((index 1))
			(lambda (x) (i2ui16 (action-record-goto-frame-index x))))

(add-action-record-type action-record-get-url
			131
			((url "") (target ""))
			(lambda (x) (xappend
				     (string2string (action-record-get-url-url x) t)
				     (string2string (action-record-get-url-target x) t))))

(add-action-record-type action-record-wait-for-frame
			138
			((index 1) (skip-count 0))
			(lambda (x) (xappend 
				     (i2ui16 (action-record-wait-for-frame-index x))
				     (i2ui8 (action-record-wait-for-frame-skip-count x)))))

(add-action-record-type action-record-set-target
			139
			((name 1))
			(lambda (x) (string2string (action-record-set-target-name x) t)))

(add-action-record-type action-record-goto-label
			140
			((name 1))
			(lambda (x) (string2string (action-record-goto-label-name x) t)))

(add-action-record-type action-record-push
			150
			((value 1))
			(lambda (x) 
			  (xappend (i2ui8 0) 
				   (string2string (action-record-push-value x) t))))

(add-action-record-type action-record-set-variable
			29
			()
			(lambda (x) nil))

