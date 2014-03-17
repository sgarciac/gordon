(use-package :gordon)

(with-movie (m "inner-contour.swf" :width 6000
	       :height 6000
	       :frame-rate 12
	       :bgcolor *white*)
    
  (add-shape m 1 1
	     (draw-shape-with-style
	      '(((200 200) (0 1000) (1000 0) (0 -1000) (-1000 0))
		((300 300) (800 0) (0 800) (-800 0) (0 -800)))
	      :fill-color *a-green*)
	     3000
	     3000)
  (add-to-movie m (make-instance 'tag-showframe))
  (add-to-movie m (make-instance 'tag-end)))
  

