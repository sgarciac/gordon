(in-package :gordon)

(let* ((width 6000)
       (height 6000)
       )
  (with-movie (m "button.swf" 
		 :width width 
		 :height height 
		 :frame-rate 12
		 :bgcolor *white*)
    (add-to-movie m (make-instance 'tag-define-shape :id 1 :shape (draw-shape-with-style
		`(,(make-circle-contour 20 500))
		:fill-color *a-lmagenta*))
	       )
    (add-to-movie m (make-instance 'tag-define-shape :id 2 :shape (draw-shape-with-style
		`(,(make-circle-contour 20 500))
		:fill-color *a-yellow*))
	       )
    (add-to-movie m
	     (make-instance 'tag-define-button-2
	      :id 3
	      :characters (list (make-instance 'button-record
				 :state-up t
				 :test-hit t
				 :character-id 1
				 :depth 1
				 :place-matrix (make-instance 'matrix)
				 :color-transform (make-instance 'cxform-wa :amult 1.0 :radd 100 :rmult 2.0 :gmult 2.0 :bmult 2.0))
				(make-instance 'button-record
				 :state-over t
				 :character-id 2
				 :depth 2
				 :place-matrix (make-instance 'matrix)
				 :color-transform (make-instance 'cxform-wa :amult 0.9 :radd 100 :rmult 2.0 :gmult 2.0 :bmult 2.0))
				(make-instance 'button-record
				 :state-down t
				 :character-id 2
				 :depth 3
				 :place-matrix (make-instance 'matrix)
				 :color-transform (make-instance 'cxform-wa :amult 0.2 :radd 10 :rmult 2.0 :gmult 2.0 :bmult 2.0))

				)))
    (add-to-movie m (make-instance 'tag-place-object2 :depth 4 :id 3 :matrix (make-instance 'matrix :translate-x 1000 :translate-y 1000)))
    (add-to-movie m (make-instance 'tag-showframe))
    (add-to-movie m (make-instance 'tag-end))))
