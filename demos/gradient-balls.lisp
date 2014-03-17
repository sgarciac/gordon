(use-package :gordon)

(let* ((width 6000)
       (height 6000)
       (radius 500)
       (grad (make-instance 'gradient
	      :record-list (list (make-instance 'gradient-record 
				  :color *yellow*)))))
  
  (with-movie (m "gradient-balls.swf" 
		 :width width 
		 :height height 
		 :frame-rate 12
		 :bgcolor *white*)
    
    (dotimes (i 20)
      (add-shape m (1+ i) (1+ i)
		 (draw-shape-with-style `(((0 0)
					   (0 ,radius ,radius 0) 
					   (,radius 0 0 ,(- radius)) 
					   (0 ,(- radius) ,(- radius) 0)
					   (,(- radius) 0 0 ,radius)))
			     
					:fill-gradient-matrix (make-instance 'matrix 
							       :scale-x 0.1 
							       :scale-y 0.1 
							       :translate-x radius
							       :translate-y 0)
			     
					:fill-gradient 
					(make-instance 'gradient 
					 :record-list `(,(make-instance 'gradient-record 
							  :ratio 0 
							  :color (random-argb))
							,(make-instance 'gradient-record 
							  :ratio 100 
							  :color (random-argb))
							,(make-instance 'gradient-record 
							  :ratio 255 
							  :color (random-argb)))))
		 (xrandom radius width) (xrandom radius height)))
    (add-to-movie m (make-instance 'tag-showframe))
    (add-to-movie m (make-instance 'tag-end))))




