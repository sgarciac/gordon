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

(genclass tag (swf-movie-object) ()
	  "The superclass for all tags.")

(defgeneric marshall-tag-body (tag)
  (:documentation
   "The generic function which oversees the marshalling of a body tag."))


(defmethod marshall ((tag tag))
  (let* ((mtd (flat-marshall-data (marshall-tag-body tag)))
         (tt (slot-value tag 'tag-id))
         (l (length mtd))
         (long? (>= l 63))
         (recordheader (if long? 
			   (list 
			    (is2leubitaba (list tt 63) '(10 6))
			    (i2ui32 l))
			   (is2leubitaba (list tt l) '(10 6)))))
    (xappend recordheader mtd)))

;;;;;;;;;;;;;;;;;;;;; ALL THE TAGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(genclass tag-end
          (tag)
          ((tag-id 0 integer :class))
          "The TAG that finishes a movie.")

(defmethod marshall-tag-body ((tag-end tag-end))
  (declare (ignore tag-end))
  *empty-ba*)




(genclass tag-enable-debugger2
          (tag)
          ((tag-id 64 integer :class))
          "The TAG that enables debugged.")

(defmethod marshall-tag-body ((tag-e-d2 tag-enable-debugger2))
  (declare (ignore tag-e-d2))
  (xappend 
   (i2ui16 0)
   (i2ui8 0)))



(genclass tag-showframe
	  (tag)
	  ((tag-id 1 integer :class))
	  "The TAG that makes the player display a new frame.")

(defmethod marshall-tag-body ((t-sf tag-showframe)) 
  (declare (ignore t-sf))
  *empty-ba*)



(genclass tag-SBC 
          (tag)
          ((tag-id 9 integer :class) bgcolor)
          "A TAG that changes the background color (an RGB) of the movie.")

(defmethod marshall-tag-body ((tag-sbc tag-sbc))
  (marshall (tag-SBC-bgcolor tag-sbc)))



(genclass tag-framelabel
	  (tag)
	  ((tag-id 43 integer :class) name)
	  "Sets a name for the current frame, so it can 
be refered later by that name.")

(defmethod marshall-tag-body ((t-fl tag-framelabel))
  (string2string (tag-framelabel-name t-fl) t))



(genclass tag-define-shape
	  (tag)
	  ((tag-id 32 integer :class) id shape)
	  "A TAG that adds a new shape to the dictionary.")

(defmethod marshall-tag-body ((t-d-s tag-define-shape))
  (with-slots
	(id shape)
      t-d-s
    (xappend
     (i2ui16 id)
     (marshall (calculate-shape-bounds shape))
     (marshall shape))))



(genclass tag-place-object2
	  (tag)
	  ((tag-id 26 integer :class) 
           depth
           id 
           move
           matrix
           cx 
           name)
	  "A TAG that places (or moves) an object from the dictionary in 
the scene.")

(defmethod marshall-tag-body ((t-p-o2 tag-place-object2))
  (with-slots (depth id move matrix cx name)
      t-p-o2
    (xappend 
     *no-bit*
     *no-bit*
     (bif name)
     *no-bit*
     (bif cx)
     (bif matrix)
     (bif id)
     (bif move)
     (i2ui16 depth)
     (when id 
       (i2ui16 id))
     (when matrix
       (marshall matrix))
     (when cx
       (marshall cx))
     (when name
       (string2string name t)))))



(genclass tag-remove-object
          (tag)
          ((tag-id 28 integer :class) 
           depth)
          "A TAG to remove the object at a particular depth, from the scene.")

(defmethod marshall-tag-body ((t-r-o tag-remove-object))
  (i2ui16 (tag-remove-object-depth t-r-o)))



(genclass tag-define-font-2
          (tag)
          ((tag-id 48 integer :class)
           (id 0 integer)
           (name "stupid-font" string)
           (offsets nil)                ;defined in defineFont
           (shapes nil)                 ;defined in defineFont
           (codes nil)
           (ascent 1000 integer)
           (descent 100 integer)
           (leading 100 integer)
           (advance 800))
          "This TAG defines a new font.")

(defmethod marshall-tag-body ((font tag-define-font-2))
  (with-slots
	(id name offsets shapes codes ascent descent leading advance)
      font
    (let ((marshalled-shapes
	   (mapcar (lambda (x) (flat-marshall-data (marshall x))) 
		   shapes)) 
	  (numglyphs (length shapes)))
      (xappend
       (i2ui16 id)
       *yes-bit*			;has layout
       *no-bit*			;JIS
       *no-bit*			;small text
       *no-bit*			;ANSI
       *yes-bit*			;wide offsets
       *yes-bit*			;wide codes
       *no-bit*			;italic
       *no-bit*			;bold
       (i2ui8 1)			;langage code latin
       (i2ui8 (array-dimension name 0))
       (string2string name)
       (i2ui16 numglyphs)		;numglyphs
       (loop
	for i = 0 then (1+ i)
	for marshalled-shape in marshalled-shapes
	collecting (+ 
		    (* 4 (1+ numglyphs))
		    total) into offsets
		    summing (length marshalled-shape) into total
		    finally (return (mapcar #'i2ui32 offsets)))
					;offsets
       (i2ui32 (+ (* 4 (1+ numglyphs)) 
		  (reduce (lambda (total item) 
			    (+ total (length (flat-marshall-data item)))) 
			  marshalled-shapes :initial-value 0))) ;codetable offset
       (apply #'xappend marshalled-shapes)
       (mapcar #'i2ui16 codes)	; the codes
		   
       (i2si16 ascent)
       (i2si16 descent)
       (i2si16 leading)
       (mapcar #'i2si16 advance)
					; (make-list numglyphs :initial-element (i2si16 (tag-define-font-2-advance font)))
       (apply #'xappend 
	      (make-list numglyphs :initial-element 
			 (xappend (marshall 
				   (make-instance 'rect 
						  :xmin 0 
						  :xmax 1000 
						  :ymin 0 
						  :ymax 100))
				  *empty-ba*)))
					;(apply #'xappend (make-list numglyphs :initial-element (marshall-rect (make-rect :xmin 0 :xmax 100 :ymin 0 :ymax 100))  ))
	   (i2ui16 0)))))



(genclass tag-define-edit-text 
	      (tag)
	      ((tag-id 37 integer :class)
	       (id nil)
	       (word-wrap t)
	       (bounds (make-instance 'rect :xmin 0 :xmax 2000 :ymin 0 :ymax 2000))
	       (password nil)
	       (editable nil)
	       (selectable nil)
	       (border nil)
	       (html nil)
	       (use-outlines t)
	       (font-id nil)
	       (font-height 12)
	       (color *a-black*)
	       (autosize nil)
	       (max-length nil)
	       (multi-lines nil)
	       (align nil)
	       (left-margin 0)
	       (right-margin 0)
	       (indent 0)
	       (leading 0)
	       (variable-name nil)
	       (initial-text "Gordon Rules"))
	      "This TAG defines a new edition box object")

(defmethod marshall-tag-body ((texto tag-define-edit-text))
  (with-slots
	(id word-wrap bounds password editable selectable border html use-outlines
		   font-id font-height color autosize max-length multi-lines
		   align left-margin right-margin indent leading variable-name
		   initial-text)
      texto
	(xappend
	 (i2ui16 id)
	 (marshall bounds)
	 *empty-ba*		    ; the former is necesary to force 
                                        ; byte-alignment when are structures 
                                        ; byte aligned? I guess they always
					; are, except if they are part
					; of an array. Im not yet sure this 
                                        ; is true, tho.
	 (bif initial-text)
	 (bif word-wrap )
	 (bif multi-lines)
	 (bif password)
	 (bif (not editable))
	 (bif color)
	 (bif max-length)
	 (bif font-id)
	 *no-bit*			; reserved
	 (bif autosize)
	 (bif (or 
	       align
	       (not (zerop left-margin))
	       (not (zerop right-margin))
	       (not (zerop indent))
	       (not (zerop leading)))) ; layout
	 (bif (not selectable))
	 (bif border)
	 *no-bit*			;reserved
	 (bif html)
	 (bif use-outlines)
	 (with id font-id 
	   (xappend 
	    (i2ui16 id)
	    (i2ui16 font-height)))
	 (with color color 
	   (marshall color))
	 (with max max-length (i2ui16 max))
	 (when (or 
		align
		(not (zerop left-margin))
		(not (zerop right-margin))
		(not (zerop indent))
		(not (zerop leading)))
	   (xappend 
	    (i2ui8 (case align
		     (:LEFT 0)
		     (:RIGHT 1)
		     (:CENTER 2)
		     (:JUSTIFY 3)
		     (nil 2)))
	    (i2ui16 left-margin)
	    (i2ui16 right-margin)
	    (i2ui16 indent)
	    (i2ui16 leading)))
	 (string2string variable-name t)
	 (string2string initial-text t))))



(genclass tag-do-action
	  (tag)
          ((tag-id 12 integer :class) records)
          "This TAG makes the player execute an action.")

(defmethod marshall-tag-body ((t-d-a tag-do-action))
  (xappend
   (apply #'xappend 
          (mapcar #'marshall-action-record (tag-do-action-records t-d-a)))
   (i2ui8 0)))




(genclass tag-define-bits
	  (tag)
	  ((tag-id 6 integer :class) 
           (id nil)
	   (jpegdata nil))
	  "This TAG creates a new object in the dictionary, 
containing a jpeg bitmap. Use tag-define-bits2 instead.")

(defmethod marshall-tag-body ((t-d-b tag-define-bits))
  (xappend
   (i2ui16 (tag-define-bits-id t-d-b))
   (tag-define-bits-jpegdata t-d-b)))



(genclass tag-define-bits2
	  (tag)
	  ((tag-id 21 integer :class) (id nil)
	   (jpegdata nil))
	  "This TAG creates a new object in the dictionary, 
containing a jpeg bitmap.")

(defmethod marshall-tag-body ((t-d-b2 tag-define-bits2))
  (xappend
   (i2ui16 (tag-define-bits2-id t-d-b2))
   (tag-define-bits2-jpegdata t-d-b2)))



(genclass tag-define-bits3
	  (tag)
	  ((tag-id 35 integer :class) (id nil)
	   (jpegdata nil)
	   (alpha nil))
	  "This TAG creates a new object in the dictionary, 
containing a jpeg bitmap, and supporting alpha channel.")

(defmethod marshall-tag-body ((t-d-b3 tag-define-bits3))
  (xappend
   (i2ui16 (tag-define-bits3-id t-d-b3))
   (i2ui32 (length (tag-define-bits3-jpegdata t-d-b3)))
   (tag-define-bits3-jpegdata t-d-b3)
   (tag-define-bits3-alpha t-d-b3)))



(genclass tag-define-button-2
	  (tag)
	  ((tag-id 34 integer :class) (id nil)
	   (menu nil)
	   (action-offset nil)
	   (characters nil)
	   (actions nil)) 
          "Creates a button object and puts it in the dictionary.")

(defmethod marshall-tag-body ((tdb tag-define-button-2))
  (with-slots
	(id menu actions characters)
      tdb
    (let* ((chars
	    (apply #'xappend (mapcar #'marshall
				     characters
				     )))
	   (marshalled-characters (flat-marshall-data chars)))
      (xappend
       (i2ui16 id)
       (i2ubita 0 7)
       (bif menu)
       (if (zerop (length actions))
	   (i2ui16 0) (i2ui16 (length marshalled-characters)))
     					;(i2ui16 0)
       marshalled-characters
       (i2ui8 0)))))



(defun make-jpeg-tag (id file)
  (with-open-file (stream file :direction :input
			  :element-type '(unsigned-byte 8))
    (let ((seq (make-array (file-length stream)
			   :element-type '(unsigned-byte 8))))
      (read-sequence seq stream)
      (make-instance 'tag-define-bits2 :id id :jpegdata seq)
      )))

(type-of (make-array 3 :element-type '(unsigned-byte 8)))
