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

(genclass movie-info (swf-movie-object)
	  ((length 0 integer)
	   (xsize 100 integer)
	   (ysize 100 integer)
	   (frame-rate 10 integer)
	   (frames-counter 0 integer)
	   ))


(genclass movie ()
	  ((frames-counter 0 integer)
	   (stream 0 stream)
	   ))

(defgeneric add-to-movie (movie object)
  (:documentation
   "The generic function which oversees adding an object to a movie."))


;;; Marshall a movie info

(defmethod marshall ((movie-info movie-info))
  (xappend
   (i2ui8 (char-code #\F))		;F
   (i2ui8 (char-code #\W))		;W
   (i2ui8 (char-code #\S))		;S 
   (i2ui8 *flash-version*)		; version 
   (i2ui32 (movie-info-length movie-info))
   (marshall (make-instance 'rect :xmin 0 :ymin 0
			    :xmax (movie-info-xsize movie-info)
			    :ymax (movie-info-ysize movie-info)))
   (i2ui8 0)				; this byte is ignored
   (i2ui8 (movie-info-frame-rate movie-info))	; rate
   (i2ui16 (movie-info-frames-counter movie-info))	; number of frames
   ))

(defmethod add-to-movie ((movie movie) (object tag))
  (write-sequence (flat-marshall-data (marshall object))
		  (movie-stream movie)))

(defmethod add-to-movie ((movie movie) (object tag-showframe))
  (incf (movie-frames-counter movie))
  (write-sequence (flat-marshall-data (marshall object))
		  (movie-stream movie)))

(defmethod add-to-movie ((movie movie) (stream stream))
  (loop with buffer = (make-array 500)
	for read-counter = (read-sequence buffer stream) then
	(read-sequence buffer stream) while (not (zerop read-counter))
	do (write-sequence buffer (movie-stream movie)
			   :start 0
			   :end read-counter)))


(defun add-shape (m id depth shape x y) 
  (add-to-movie m (make-instance 'tag-define-shape 
	      :id id
	      :shape shape))

  (add-to-movie m (make-instance 'tag-place-object2 
	      :depth depth
	      :id id
	      :matrix (make-instance 'matrix
				     :translate-x x
				     :translate-y y))))


(defmacro with-movie 
    ((varname filename &key (frame-rate 12) (width 10000) (height 10000)
	      (bgcolor nil) (debug nil)) &body body)
  (with-gensyms (outputstream movie-info movie-length) 
    `(with-open-file (,outputstream ,filename
		      :direction :output 
		      :if-exists :supersede
		      :element-type '(unsigned-byte 8))
      (let ((,varname 
	     (make-instance 
	      'movie :stream ,outputstream
	      ))
	    (,movie-info
	     (make-instance
	      'movie-info :xsize ,width
	      :ysize ,height
	      :frame-rate ,frame-rate)))
	(write-sequence
	 (flat-marshall-data (marshall ,movie-info))
	 (movie-stream ,varname))
	,@(if bgcolor `((add-to-movie
			 ,varname
			 (make-instance 'tag-SBC :bgcolor ,bgcolor))))
	,@(if debug `((add-to-movie
			 ,varname
			 (make-instance 'tag-enable-debugger2))))
	,@body
	(let ((,movie-length (file-position (movie-stream ,varname))))
	  (file-position (movie-stream ,varname) 0)
	  (setf (movie-info-frames-counter ,movie-info)
		(movie-frames-counter ,varname))
	  (setf (movie-info-length ,movie-info)
		,movie-length)
	  (write-sequence
	   (flat-marshall-data (marshall ,movie-info))
	   (movie-stream ,varname))
	  )))))


