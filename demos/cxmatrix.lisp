(use-package :gordon)

(let* ((swf "sergio.swf")
       (width 279)
       (height 457)
       (img1 '(:file "sergio.jpg" :width 279 :height 457))
       (a-white (make-instance 'rgba :r 255 :g 255 :b 255 :a 10)))
  (with-movie (m swf :width width :height height :frame-rate 10 :bgcolor *white*)

    (add-to-movie m (make-jpeg-tag
                     1 (getf img1 :file)))
    (add-to-movie m (make-instance 'tag-define-shape :id 2
                                   :shape (draw-shape-with-style
                                           `(,(make-rect-contour
                                               (getf img1 :width)
                                               (getf img1 :height)))
                                           :fill-color *a-blue*
                                           :stroke-width 1
                                           :stroke-color a-white
                                           :fill-bitmap 1)))
    (add-to-movie m (make-instance 'tag-place-object2 :id 2 :depth 1))
    (dotimes (i 100) 
      (add-to-movie m (make-instance 'tag-place-object2
				     :move t
				     :depth 1
				     :cx (make-instance 'cxform-wa
							:amult (coerce (/ 1 (+ 1 i)) 'float))))

      (add-to-movie m (make-instance 'tag-showframe)))
    
    
    
    (add-to-movie m (make-instance 'tag-end))))


 






