(use-package :gordon)

(let* ((width 6000)
       (height 6000)
       (radius 500))
  (with-movie (m "image.swf" 
		 :width width 
		 :height height 
		 :frame-rate 12
		 :bgcolor *black*)
    (add-to-movie m (make-jpeg-tag 1 "azul.jpeg"))
    (add-shape m 2 2
	       (draw-shape-with-style
		`(,(make-circle-contour 10 1000))
		:fill-bitmap 1
		:fill-bitmap-style *repeating-bitmap-fill*)
	       2000 2000)
    (loop for i from 0 upto 200 do
	  (progn 
	    (add-to-movie m
		     (make-instance 'tag-place-object2
		      :depth 2 :move t :matrix
		      (+-matrix (make-rotation-matrix-angle (* 0.05 i))
				(make-instance 'matrix :scale-x (* 0.1 i) :scale-y (* i 0.1) :rotate-skew-0 0.0 :rotate-skew-1 0.0 :translate-x 2000 :translate-y (* -1 (* 3 i))))

		      ))
	    (add-to-movie m (make-instance 'tag-showframe))))
    
    (add-to-movie m (make-instance 'tag-end))))




