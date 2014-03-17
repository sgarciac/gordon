(use-package :gordon)

(with-movie (m "simple-crossed.swf" 
	       :width 5000
	       :height 5000 
	       :frame-rate 12
	       :bgcolor *green*)
  (add-shape m 1 1
	     (draw-shape-with-style `((
				       (0 0)
				       (0 2000)
				       (2000 0)
				       (0 -2000)
				       (-2000 0)
				       ) 
				      (
				       (1000 1000)
				       (0 2000)
				       (2000 0)
				       (0 -2000)
				       (-2000 0)
				       )
				      )
				    :fill-color *a-red*)
	     0 0)
	     

  (add-to-movie m (make-instance 'tag-end)))



