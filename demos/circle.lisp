(use-package :gordon)

(let* ((width 6000)
       (height 6000)
       (radius 500))
  (with-movie (m "circle.swf" 
		 :width width 
		 :height height 
		 :frame-rate 12
		 :bgcolor *white*)
    (add-shape m 1 1
	       (draw-shape-with-style `(,(make-circle-contour 20 200))
				      :fill-color *a-red*)
	       1000 1000)
    (add-to-movie m (make-instance 'tag-showframe))
    (add-to-movie m (make-instance 'tag-end))))

