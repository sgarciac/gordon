(use-package :gordon
 )

(with-movie (m "line.swf" 
	       :width 5000
	       :height 5000 
	       :frame-rate 12
	       :bgcolor *white*)
  (add-shape m 1 1
	     (draw-shape-with-style `((
				       (0 0)
				       (5000 5000)
				       )))
	     0 0)

  (add-to-movie m (make-instance 
	      'tag-end)))
