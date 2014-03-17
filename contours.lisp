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

(defun draw-shape-with-style (contours
			      &key 
			      (fill-gradient-matrix nil)
			      (fill-gradient nil)
			      (fill-bitmap nil)
			      (fill-bitmap-style *clipped-bitmap-fill*)
			      (fill-bitmap-matrix (make-instance 'matrix))
			      (fill-color *a-white*)
			      (stroke-color *a-black*)
			      (stroke-width 20))
  
  "Creates a flash shape from a list of contours. A contour is a list of straight lines and curved lines, defined as a list of steps. An step is either an (xdelta ydelta) delta or an (xcontol ycontrol xdelta ydelta). The first step is always a (x y) and signals the start point of the contour. Draw-shape will not verify that the contours are closed."
  (make-instance
   'shape-with-style 
   :fillstyles (make-instance
		'fillstyle-array 
		:styles (cond ((and fill-gradient-matrix fill-gradient)
			       (list
				(make-instance 'fillstyle
					       :type *radial-gradient-fill*
					       :gradient-matrix fill-gradient-matrix
					       :gradient fill-gradient) 
				))
			      (fill-bitmap
			       (list (make-instance 'fillstyle :type
						    fill-bitmap-style
						    :bitmap-id fill-bitmap
						    :bitmap-matrix fill-bitmap-matrix
						    )))
			      
			      (t (list (make-instance 'fillstyle :color fill-color)))))
   
   
   :linestyles (make-instance 'linestyle-array 
			      :styles `(,(make-instance 'linestyle 
							:width stroke-width 
							:color stroke-color)))
   
   :records (apply #'append (mapcar #'make-records-from-contour contours))))

(defun curved-edge-p (x)
  (= 4 (length x)))

(defun straight-edge-p (x)
  (= 2 (length x)))

(defun make-records-from-contour (contour &key (linestyle 1) (fillstyle0 1) (fillstyle1 0)) 
  (labels ((make-record (x)
	     (when (or 
		    (not (or (straight-edge-p x) (curved-edge-p x)))
		    (not (find-if (lambda (it) (not (zerop it))) x)))
	       (format t "Corrupt step in contour?: ~A" x))
	     (if (straight-edge-p x)
		 (make-instance 'straight-edge-record 
		  :delta-x (first x) 
		  :delta-y (second x))
		 (make-instance 'curved-edge-record  
		  :control-delta-x (first x) 
		  :control-delta-y (second x) 
		  :anchor-delta-x (third x) 
		  :anchor-delta-y (fourth x)))))
    
    (cons (make-instance 'style-change-record :delta-x (first (first contour)) 
				    :delta-y (second (first contour)) 
				    :linestyle linestyle
				    :fillstyle0 fillstyle0 
				    :fillstyle1 fillstyle1)

	  (mapcar #'make-record (cdr contour)))))

(defun make-polygone-contour (n l)
  (cons '(0 0)
	(do ((i 0 (1+ i))
	     (angle 0 (+ angle (/ (* 2 pi) n)))
	     (sides `() (append sides (list (list (round (* l (sin angle))) (round (* l (cos angle))))))))
	    ((= i n) sides))))


(defun make-arch (segments-count radio angle &key (initial-angle 0))
  (labels ((make-segment (distance total-angle angle reminders)
	     (multiple-value-bind (xcontrol-value xcontrol-reminder)
		 (round (+ (first reminders ) (* distance (cos total-angle))))
	       (multiple-value-bind (ycontrol-value ycontrol-reminder)
		   (round (+ (second reminders) (* distance (sin total-angle))))
		 (multiple-value-bind (xdelta-value xdelta-reminder)
		     (round (+ (third reminders) (* distance (cos (+ angle total-angle)))))
		   (multiple-value-bind (ydelta-value ydelta-reminder)
		       (round (+ (fourth reminders) (* distance (sin (+ angle total-angle)))))
		     (cons (list xcontrol-value ycontrol-value xdelta-value ydelta-value)
			     (list xcontrol-reminder ycontrol-reminder xdelta-reminder ydelta-reminder))))))))
    (let*
	((segment-angle (/ angle segments-count))
	 (distance (* radio (tan (/ segment-angle 2)))))
      (loop
       with reminders = (list 0 0 0 0)
       for i from 1 upto segments-count 
       for total-angle = initial-angle then (+ total-angle segment-angle) 
       for calculate-segment = (make-segment distance total-angle segment-angle reminders)
       do (setf reminders (cdr calculate-segment))
       collecting (car calculate-segment)))))

(defun make-circle-contour (n r)
  "Creates a circular contour of radio r, using n steps"
  (cons '(0 0) (make-arch n r (* 2 pi))))

(defun make-rect-contour (w h)
  "Creates a rectangular contour of width w and height h"
  `((0 0) (,w 0) (0 ,h) (,(- w) 0) (0 ,(- h))))

(defun make-bridge-contour (steps radio angle width &key (initial-angle 0)) 
  (close-contour 
  (append 
   `((,(round (* radio (sin initial-angle))) ,(- (round (* radio (cos initial-angle))))))
   (make-arch steps radio angle :initial-angle initial-angle)
   `((,(round (* width (cos (+ initial-angle angle (/ pi 2))))) ,(round (* width (sin (+ initial-angle angle (/ pi 2)))))))
   (make-arch steps (- radio width) (* -1 angle) :initial-angle (- (+ initial-angle angle) (* 2 pi)))
   `((,(round (* width (cos (- initial-angle (/ pi 2))))) ,(round (* width (sin (- initial-angle (/ pi 2))))))))))


(defun closed-p (contour)
  (let ((delta
	 (reduce (lambda (total step) 
		   (if (straight-edge-p step) 
		       (cons (- (car total) (first step))
			     (- (cdr total) (second step)))
		       (cons (- (car total) (first step) (third step))
			     (- (cdr total) (second step) (fourth step))))) 
		 (cdr contour) :initial-value '(0 . 0))))
    (= 0  (car delta) (cdr delta))))

(defun add-joint-to-contour (contour) 
  "given a contour, returns a contour that is closed, adding an straight edge if necessary"
  (let ((delta
	 (reduce (lambda (total step) 
		   (if (straight-edge-p step) 
		       (cons (- (car total) (first step))
			     (- (cdr total) (second step)))
		       (cons (- (car total) (first step) (third step))
			     (- (cdr total) (second step) (fourth step))))) 
		 (cdr contour) :initial-value '(0 . 0))))
    (if (not (= 0  (car delta) (cdr delta)))
	(append contour (list (list (car delta) (cdr delta))))
	contour)))


(defun close-contour (contour) 
  "given a contour, returns a contour that is closed, by changing the last edge"
  (let ((delta
	 (reduce (lambda (total step) 
		   (if (straight-edge-p step) 
		       (cons (- (car total) (first step))
			     (- (cdr total) (second step)))
		       (cons (- (car total) (first step) (third step))
			     (- (cdr total) (second step) (fourth step))))) 
		 (cdr contour) :initial-value '(0 . 0)))
	(last-edge (car (last contour))))
	
    (if (not (= 0  (car delta) (cdr delta)))
	(append (subseq contour 0 (- (length contour) 1))
		(if (curved-edge-p last-edge)
		    (list (list (first last-edge) (second last-edge)
				(+ (car delta) (third last-edge))
				(+ (cdr delta) (fourth last-edge))))
		    (list (list (+ (first last-edge) (car delta))
				(+ (second last-edge) (cdr delta))))))
	contour)))


(defun invert-contour-vertical (contour)
  (mapcar (lambda (step) (mapcar-pos (lambda (item pos) (if (oddp pos) (* -1 item) item)) step)) contour))

(defun scale-contour (contour xscale yscale)
  (let ((new-contour 
	 (mapcar (lambda (step) (if (straight-edge-p step)
				    (list (floor (* xscale (first step)))
					  (floor (* yscale (second step))))
				    (list (floor (* xscale (first step)))
					  (floor (* yscale (second step)))
					  (floor (* xscale (third step)))
					  (floor (* yscale (fourth step))))))
		 contour)))
    (if (closed-p contour)
	(close-contour new-contour)
	new-contour)))




(defun scale-contours (contours xscale yscale)
  (mapcar (lambda (contour) (scale-contour contour xscale yscale)) contours))

(defun final-point (contour)
  (reduce (lambda (current step) (if (curved-edge-p step) 
				     (cons (+ (car current) (first step) (third step))
					   (+ (cdr current) (second step) (fourth step)))
				     (cons (+ (car current) (first step))
					   (+ (cdr current) (second step))))) 
	  contour 
	  :initial-value '(0 . 0)))



