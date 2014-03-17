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

;;;; Structures of "higher" SWF types
(genclass color (swf-movie-object)() "The superclass for all swf colors")
(genclass shape-record ()() "The superclass for all shape records.")

(genclass shape
          (swf-movie-object)
          ((records ()(or list null)))
          "The superclass for all swf shapes")

(genclass rect
          (swf-movie-object)
          ((xmin 0 integer)
           (xmax 0 integer)
           (ymin 0 integer)
           (ymax 0 integer))
          "The basic construct for rectangles consisting of four integer coordinates.  This is not a swf shape in that it doesn't contain shape-records.")

(genclass rgb
          (color)
          ((r 0 integer)
           (g 0 integer)
           (b 0 integer))
          "The basic construct for color representation consisting of three color values (red, green, and blue).")

(genclass rgba
          (rgb)
          ((a 0 integer))
          "The basic construct for color representation consisting of three colors (red, green, and blue) and an alpha channel.")

;; color transformation with alpha
(genclass cxform-wa
	  (swf-movie-object)
	  ((rmult 1.0 float)
           (gmult 1.0 float)
           (bmult 1.0 float)
           (amult 1.0 float)
           (radd 0 integer)
           (gadd 0 integer)
           (badd 0 integer)
           (aadd 0 integer))
          "The basic construct for transforming colors by multiples or integer values.")

(defmethod cxform-has-mult ((cx cxform-wa))
  (not (and (= (cxform-wa-rmult cx) 1)
	    (= (cxform-wa-gmult cx) 1)
	    (= (cxform-wa-bmult cx) 1)
	    (= (cxform-wa-amult cx) 1))))

(defmethod cxform-has-add ((cx cxform-wa))
  (not (and (zerop (cxform-wa-radd cx))
	    (zerop (cxform-wa-gadd cx))
	    (zerop (cxform-wa-badd cx))
	    (zerop (cxform-wa-aadd cx)))))

(defmethod marshall ((cx cxform-wa))
  (let ((hm (cxform-has-mult cx))
	(hd (cxform-has-add cx)))
    (xappend
     (bif hd)
     (bif hm)
     (when (or hm hd)
       (is2besbitawithmax 
	(append
	 (when hm (mapcar (lambda (n) (floor (* 256 n))) 
			  `(,(cxform-wa-rmult cx)
			    ,(cxform-wa-gmult cx)
			    ,(cxform-wa-bmult cx)
			    ,(cxform-wa-amult cx))))
	 (when hd `(,(cxform-wa-radd cx)
		    ,(cxform-wa-gadd cx)
		    ,(cxform-wa-badd cx)
		    ,(cxform-wa-aadd cx))))
	4)))))

(genclass matrix
          (swf-movie-object)
          ((scale-x 1.0 float)
           (scale-y 1.0 float)
           (rotate-skew-0 0.0 float)
           (rotate-skew-1 0.0 float)
           (translate-x 0 integer)
           (translate-y 0 integer))
          "A two dimensional matrix which can be scaled, skewed, or translated in both x and y directions.")

(genclass gradient-record
	  (swf-movie-object)
	  ((ratio 1 integer)
	   (color (make-instance 'rgb) color))
	  "An individual record of color for use in the description of a color gradient.")

(genclass gradient
	  (swf-movie-object)
	  ((record-list () list))
	  "A description of a gradient color pattern.")

; Shape styles

(genclass fillstyle
	  (swf-movie-object)
	  ((type *solid-fill* integer)
	   (color nil (or color null))
	   (gradient-matrix nil (or matrix null))
	   (gradient nil (or gradient null))
	   (bitmap-id nil (or integer null))
	   (bitmap-matrix nil (or matrix null)))
	  "A description of how a shaped is filled.  Colors, color gradients, and bitmaps can be used in this description.")

(genclass linestyle
	  (swf-movie-object)
	  ((width 1 integer)
	   (color nil (or color null)))
	  "A description of how a line is rendered.  The line width and color are part of this description.")

(genclass fillstyle-array
	  (swf-movie-object)
	  ((styles () list)) 
	  "A container for fillstyles.")

(genclass linestyle-array 
	  (swf-movie-object)
	  ((styles () list)) 
	  "A container for linestyles.")

(genclass style-change-record
	  (shape-record)
	  ((delta-x 0 integer)
	   (delta-y 0 integer)
	   (fillstyle0 nil (or null integer))
	   (fillstyle1 nil (or null integer))
	   (linestyle nil (or null integer)))
	  "A record used to indicate a style change either in x/y position, fillstyle or linestyle.")

(genclass straight-edge-record
	  (shape-record)
	  ((delta-x 0 integer)
	   (delta-y 0 integer))
	  "A record representing the coordinates used in a straight line.")

(genclass curved-edge-record
	  (shape-record)
	  ((control-delta-x 0 integer)
	   (control-delta-y 0 integer)
	   (anchor-delta-x 0 integer)
	   (anchor-delta-y 0 integer))
	  "A record representing the coordinates used in a curved line.")

(genclass end-record
	  (shape-record)
	  ()
	  "The closing record.")

(genclass shape-with-style
	  (shape)
	  ((fillstyles nil (or fillstyle-array null))
	   (linestyles nil (or linestyle-array null)))
	  "A description of a shape which includes fillstyle and linestyle descriptions.")

(genclass button-record
	  (swf-movie-object)
	  ((test-hit)
	   (state-down)
	   (state-over)
	   (state-up)
	   (character-id)
	   (depth)
	   (place-matrix nil matrix)
	   (color-transform nil cxform-wa))
	  "A record describing one aspect of a button.")

(defgeneric calculate-shape-bounds (shape)
  (:documentation "Calculate the minimum rectangle (with sides parallel to the axis) that contains a shape. This is used by the DEFINE SHAPE TAG. If no rectangle is defined, the refresh may fail."))

(genclass xy-accum
		()
		((x 0 integer)
		 (y 0 integer))
		"A helper class used for accumulating bounds information.")

(defgeneric calculate-shape-record-bounds (record rect xy-accum)
  (:documentation "A helper function to calculate a shape record's bounds."))

(defmethod calculate-shape-bounds ((shape shape))
  (let ((bounds (make-instance 'rect))
	(accum (make-instance 'xy-accum)))
    (dolist (record (shape-records shape) bounds)
      (calculate-shape-record-bounds record bounds accum))))

(defmethod calculate-shape-record-bounds ((record style-change-record)
					  (rect rect)
					  (accum xy-accum))
  (with-slots (x y) accum
    (with-slots (xmin xmax ymin ymax) rect
      (setf x (+ x (style-change-record-delta-x record)))
      (setf y (+ y (style-change-record-delta-y record)))
      (setf xmin (min x xmin))
      (setf xmax (max x xmax))
      (setf ymin (min y ymin))
      (setf ymax (max y ymax)))))

(defmethod calculate-shape-record-bounds ((record straight-edge-record)
					  (rect rect)
					  (accum xy-accum))
  (with-slots (x y) accum
    (with-slots (xmin xmax ymin ymax) rect
      (setf x (+ x (straight-edge-record-delta-x record)))
      (setf y (+ y (straight-edge-record-delta-y record)))
      (setf xmin (min x xmin))
      (setf xmax (max x xmax))
      (setf ymin (min y ymin))
      (setf ymax (max y ymax)))))

(defmethod calculate-shape-record-bounds ((record curved-edge-record)
					  (rect rect)
					  (accum xy-accum))
  (with-slots (x y) accum
    (with-slots (xmin xmax ymin ymax) rect
      (let ((ix (+ x (curved-edge-record-control-delta-x record)))
	    (iy (+ y (curved-edge-record-control-delta-y record))))
	(setf x (+ ix (curved-edge-record-anchor-delta-x record)))
	(setf y (+ iy (curved-edge-record-anchor-delta-y record)))
	(setf xmin (min x ix xmin))
	(setf xmax (max x ix xmax))
	(setf ymin (min y iy ymin))
	(setf ymax (max y iy ymax))))))

;;;; Multiply transformation matrix
(defun make-rotation-matrix-angle (angle)
  (make-instance 'matrix
		 :scale-x (cos angle)
		 :scale-y (cos angle)
		 :rotate-skew-0 (* -1 (sin angle))
		 :rotate-skew-1 (sin angle)))


(defun +-matrix (matrix1 matrix2)
  (make-instance 'matrix
		 :scale-x (+ (matrix-scale-x matrix1) (matrix-scale-x matrix2))
		 :scale-y (+ (matrix-scale-y matrix1) (matrix-scale-y matrix2))
		 :rotate-skew-0 (+ (matrix-rotate-skew-0 matrix1) (matrix-rotate-skew-0 matrix2))
		 :rotate-skew-1 (+ (matrix-rotate-skew-1 matrix1) (matrix-rotate-skew-1 matrix2))
		 :translate-x (+ (matrix-translate-x matrix1) (matrix-translate-x matrix2))
		 :translate-y (+ (matrix-translate-y matrix1) (matrix-translate-y matrix2))))

(defun *-matrix (matrix1 matrix2)
  (make-instance 'matrix
		 :scale-x (+ (* (matrix-scale-x matrix1) (matrix-scale-x matrix2))
	       (* (matrix-rotate-skew-0 matrix1) (matrix-rotate-skew-1 matrix2)))
		 :scale-y (+ (* (matrix-rotate-skew-1 matrix1) (matrix-rotate-skew-0 matrix2))
			     (* (matrix-scale-y matrix1) (matrix-scale-y matrix2)))
		 :rotate-skew-0 (+ (* (matrix-scale-x matrix1) (matrix-rotate-skew-0 matrix2))
				   (* (matrix-rotate-skew-0 matrix1) (matrix-scale-y matrix2)))
		 :rotate-skew-1 (+ (* (matrix-rotate-skew-1 matrix1) (matrix-scale-x matrix2))
				   (* (matrix-scale-x matrix1) (matrix-scale-y matrix2)))
		 :translate-x (floor (+ (* (matrix-scale-x matrix1) (matrix-translate-x matrix2))
			   (* (matrix-rotate-skew-0 matrix1) (matrix-translate-y matrix2))
			   (matrix-translate-x matrix1)))
   :translate-y (floor (+ (* (matrix-scale-y matrix1) (matrix-translate-y matrix2))
			   (* (matrix-rotate-skew-1 matrix1) (matrix-translate-x matrix2))
			   (matrix-translate-y matrix1)))))

;;;;
;;;; Marshalling of SWF objects
;;;; the type returned for all marshall functions is
;;;; a list that contains byte arrays and bit arrays
;;;; 

;;; this functions takes a list that represents marshalled data, and 
;;; returns the corresponsing SWF byte array, byte-aligned

(defun flat-marshall-data (md)
  (apply #'concba (nreverse 
		   (reduce #'push-val (xappend md (list *empty-ba*)) :initial-value '())
		   )))

;(defun push-val (lst val) 
;  (cond ((typep (car lst) '(or null byte-array)) (push val lst))
;	((and (typep (car lst) 'bit-array) (typep val 'bit-array))
;	 (push-val (cdr lst) (concbita (car lst) val)))
;	((and (typep (car lst) 'bit-array) (typep val 'byte-array))
;	 (push-val (cdr lst) (concba (bita2beba (car lst)) val)))
;	(t (error (format nil "unrecognized type ~A ~A " (type-of (car lst)) (type-of val))))))

(defun push-val (lst val) 
  (cond ((typep (car lst) '(or null byte-array)) (push val lst))
	((and (typep (car lst) 'bit-array) (typep val 'bit-array))
	 (let ((ato (concbita (pop lst) val)))
	   (push-val lst ato)))
	((and (typep (car lst) 'bit-array) (typep val 'byte-array))
	 (let ((ato (bita2beba (pop lst))))
	   (push-val lst (concba ato val))))
	(t (error (format nil "unrecognized type ~A ~A " (type-of (car lst)) (type-of val))))))

(defmethod marshall ((rect rect))
  (xappend (is2besbitawithmax
	    `(,(rect-xmin rect)
	      ,(rect-xmax rect)
	      ,(rect-ymin rect)
	      ,(rect-ymax rect))
	    5)))

(defmethod marshall ((col rgb))
  (xappend (i2ui8 (rgb-r col))
	  (i2ui8 (rgb-g col))
	  (i2ui8 (rgb-b col))))

(defmethod marshall ((col rgba))
  (xappend (call-next-method)
	   (i2ui8 (rgba-a col))))

(defmethod marshall ((gr gradient-record))
  (xappend (i2ui8 (gradient-record-ratio gr))
	  (marshall (gradient-record-color gr))))

(defmethod marshall ((g gradient))
  (apply #'xappend (i2ui8 (length (gradient-record-list g)))
	 `(,@(mapcar #'marshall (gradient-record-list g)))))

(defmethod marshall ((br button-record))
  (xappend
   (i2ubita 0 4)
   (bif (button-record-test-hit br))
   (bif (button-record-state-down br))
   (bif (button-record-state-over br))
   (bif (button-record-state-up br))
   (i2ui16 (button-record-character-id br))
   (i2ui16 (button-record-depth br))
   (marshall (button-record-place-matrix br))
   *empty-ba*
   (marshall (button-record-color-transform br))
   *empty-ba*))

(defmethod marshall ((fs fillstyle))
  (xappend
   (i2ui8 (fillstyle-type fs))
   (if (= (fillstyle-type fs) *solid-fill*)
       (if (fillstyle-color fs)
           (marshall (fillstyle-color fs)))
       *empty-ba*)
   (if (or (= (fillstyle-type fs) *linear-gradient-fill*)
	   (= (fillstyle-type fs) *radial-gradient-fill*))
       (xappend (marshall (fillstyle-gradient-matrix fs))
		(marshall (fillstyle-gradient fs)))
       (xappend *empty-bita* *empty-ba*))
   (if (find (fillstyle-type fs) `(,*repeating-bitmap-fill* 
				   ,*clipped-bitmap-fill*
				   ,*non-smoothed-repeating-bitmap*
				   ,*non-smoothed-clipped-bitmap*))
       (xappend (i2ui16 (fillstyle-bitmap-id fs))
		(marshall (fillstyle-bitmap-matrix fs))
		*empty-ba*)
       (xappend  *empty-bita* *empty-ba*))))

(defmethod marshall ((fa fillstyle-array))
  (let ((styles (fillstyle-array-styles fa)))
    (xappend 
     (if (< (length styles) 255)
	 (i2ui8 (length styles))
	 (xappend (i2ui8 255) (i2ui16 (length styles))))
     (apply #'xappend 
            (mapcar #'(lambda (x)(marshall x)) styles)))))

(defmethod marshall ((ls linestyle))
  (xappend (i2ui16 (linestyle-width ls))
           (if (linestyle-color ls)
               (marshall (linestyle-color ls)))))

(defmethod marshall ((la linestyle-array))
  (let ((styles (linestyle-array-styles la)))
    (xappend 
     (if (< (length styles) 255)
	 (i2ui8 (length styles))
	 (xappend (i2ui8 255) (i2ui16 (length styles))))
     (apply #'xappend 
            (mapcar #'(lambda (x)(marshall x)) styles)))))

;;; marshall a matrix
(defmethod matrix-has-scale ((m matrix))
  (not (= 1 (matrix-scale-x m) (matrix-scale-y m))))


(defmethod matrix-has-rotation ((m matrix))
  (not (= 0 (matrix-rotate-skew-1 m) (matrix-rotate-skew-0 m))))


(defmethod matrix-has-translation ((m matrix))
  (not (= 0 (matrix-translate-x m) (matrix-translate-y m))))

(defmethod marshall ((m matrix))
  (xappend
   (if (matrix-has-scale m)
       (xappend *yes-bit*
		 (fs2besbitawithmax 
		  `(,(matrix-scale-x m)
		    ,(matrix-scale-y m)) 5))
       *no-bit*)
   (if (matrix-has-rotation m)
       (xappend *yes-bit*
		 (fs2besbitawithmax 
		  `(,(matrix-rotate-skew-0 m)
		    ,(matrix-rotate-skew-1 m)) 5))
       *no-bit*)
   ;;(if (matrix-has-translation m)
   ;;    (xappend *yes-bit*
   (is2besbitawithmax 
    `(,(matrix-translate-x m)
      ,(matrix-translate-y m)) 5)))
    ;;   *no-bit*)))

;;; shapes marshalling

(defun style-change-record-has-delta (sc)
  (or
   (not (zerop (style-change-record-delta-x sc)))
   (not (zerop (style-change-record-delta-y sc)))))

(defmethod marshall ((sr style-change-record))
  (xappend 
   *no-bit*                             ;no edge
   *no-bit*                             ;no new styles
   (bif (style-change-record-linestyle sr))
   (bif (style-change-record-fillstyle1 sr))
   (bif (style-change-record-fillstyle0 sr))
   (if (style-change-record-has-delta sr)
       (xappend *yes-bit*
                (is2besbitawithmax `(,(style-change-record-delta-x sr)
                                     ,(style-change-record-delta-y sr))
                                   5))
       *no-bit*)
   (when (style-change-record-fillstyle0 sr)
     (i2ubita (style-change-record-fillstyle0 sr) *fillbits*))
   (when (style-change-record-fillstyle1 sr)
     (i2ubita (style-change-record-fillstyle1 sr) *fillbits*))
   (when (style-change-record-linestyle sr)
     (i2ubita (style-change-record-linestyle sr) *linebits*))))

(defmethod marshall ((end-record end-record))
  (declare (ignore end-record))
  (xappend
   *no-bit*
   (i2ubita 0 5)))

(defmethod marshall ((er curved-edge-record))
  (let ((neededsize  (max
                      (needsize (curved-edge-record-control-delta-x er) t)
                      (needsize (curved-edge-record-control-delta-y er) t)
                      (needsize (curved-edge-record-anchor-delta-x er) t)
                      (needsize (curved-edge-record-anchor-delta-y er) t))))
    (xappend 
     *yes-bit*                          ;edge
     *no-bit*                           ;not straight
     (i2ubita (- neededsize 2) 4)
     (i2sbita (curved-edge-record-control-delta-x er) neededsize)
     (i2sbita (curved-edge-record-control-delta-y er) neededsize)
     (i2sbita (curved-edge-record-anchor-delta-x er) neededsize)
     (i2sbita (curved-edge-record-anchor-delta-y er) neededsize))))

(defmethod marshall ((er straight-edge-record))
  (let ((neededsize  (max 
		      2
		      (needsize (straight-edge-record-delta-x er) t)
		      (needsize (straight-edge-record-delta-y er) t))))
    
    (xappend 
     *yes-bit*                          ;edge record
     *yes-bit*                          ;straight
     (i2ubita (- neededsize 2) 4)
     (cond ((not (or (zerop (straight-edge-record-delta-x er))
		     (zerop (straight-edge-record-delta-y er))))
	    (xappend 
	     *yes-bit*
	     (i2sbita (straight-edge-record-delta-x er) neededsize)
	     (i2sbita (straight-edge-record-delta-y er) neededsize)))
	   ((zerop (straight-edge-record-delta-x er))
	    (xappend
	     *no-bit*
	     *yes-bit*
	     (i2sbita (straight-edge-record-delta-y er) neededsize)))
	   (t 
	    (xappend
	     *no-bit*
	     *no-bit*
	     (i2sbita (straight-edge-record-delta-x er) neededsize)))))))



(defmethod marshall ((shp shape))
  (let* ((*fillbits* (needsize 1 nil))
	 (*linebits* (needsize 1 nil)))
    (declare (special *fillbits* *linebits*))
    (xappend
     (i2ubita *fillbits* 4)
     (i2ubita *linebits* 4)
     (apply #'xappend 
            (mapcar (lambda (x) (marshall x))
		    
                    (append (shape-records shp)
                            (list (make-instance 'end-record))))))))

(defmethod marshall ((sws shape-with-style))
  (let* ((fs-count 
          (length (fillstyle-array-styles (shape-with-style-fillstyles sws))))
	 (ls-count 
          (length (linestyle-array-styles (shape-with-style-linestyles sws))))
	 (*fillbits* (needsize fs-count nil))
	 (*linebits* (needsize ls-count nil)))
    (declare (special *fillbits* *linebits*))
    (xappend
     (marshall (shape-with-style-fillstyles sws))
     (marshall (shape-with-style-linestyles sws))
     (i2ubita *fillbits* 4)
     (i2ubita *linebits* 4)
     (apply #'xappend 
            (mapcar (lambda (x) (marshall x))
		    (append (shape-records sws) 
                            (list (make-instance 'end-record))))))))

