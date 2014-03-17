(use-package :gordon)

(defun make-curved-star-path (n l c)
  (close-contour
   (cons '(0 0)
	 (do ((i 0 (1+ i))
	      (angle 0 (+ angle (/ (* 2 pi) n)))
	      (sides `() 
		     (let* ((lprima (sqrt (+ (expt (/ l 2) 2) (expt c 2))))
			    (gamma (- angle (atan (/ c (/ l 2)))))
			    (x1 (* lprima (sin gamma)))
			    (y1 (* lprima (cos gamma))))
		       
		       (append sides (list (list 
					    (round x1)
					    (round y1)
					    (round (- (* l (sin angle)) x1))
					    (round (- (* l (cos angle)) y1))
					    ))))))
	     ((= i n) sides)))))
					

(let* ((width 6000)
       (height 6000)
       (nshapes 50)
       (sizelength 200)
       (maxsides 8)
       (downtimes 10)
       (xs (random-vector nshapes 0 width))
       (ys (random-vector nshapes 0 height)))

  (with-movie (m "falling-stars.swf" :width width 
		 :height height 
		 :frame-rate 12
		 :bgcolor *white*)

    ;; create the curved stars and place them randomly

    (dotimes (i nshapes)
      (add-shape m (1+ i) (1+ i)
		 (draw-shape-with-style 
		  (list (make-curved-star-path 
			 (xrandom 3 maxsides)
			 (+ (* i 5) sizelength)
			 (+ (* i 5) 1000)))
		  :fill-color (random-argb))
		 (aref xs i)
		 (aref ys i))		    
      (add-to-movie m (make-instance 'tag-showframe)))
    
    ;; make the stars fall
    (dotimes (i nshapes)
      (dotimes (j downtimes)
	(add-to-movie m (make-instance 'tag-place-object2 
		    :move t
		    :depth (- nshapes i) 
		    :matrix (translation-matrix (aref xs (- nshapes (1+ i)))
						(+ (aref ys (- nshapes (1+ i))) 
						   (* j (/ height downtimes))))))
	(add-to-movie m (make-instance 'tag-showframe))))
    (add-to-movie m (make-instance 'tag-end))))


