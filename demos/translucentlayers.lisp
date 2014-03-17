; Contributed by Jeff Cunningham

(use-package :gordon)


(let* ((swf "flasher.swf")
       (width 279)
       (height 457)
       (img1 '(:file "sergio.jpg" :width 279 :height 457))
       (a-white (make-instance 'rgba :r 255 :g 255 :b 255 :a 255))
       )
  (with-movie (m swf :width width :height height :frame-rate 15 :bgcolor *white*)

    (add-to-movie m (make-jpeg-tag 1 (getf img1 :file)))
    (add-to-movie m (make-instance 'tag-define-shape :id 3
                                   :shape (draw-shape-with-style
                                           `(,(make-rect-contour (getf img1 :width) (getf img1 :height)))
                                           :stroke-width 1
                                           :stroke-color a-white
                                           :fill-bitmap 1
                                           )))

    ;; Fade is obtained here by adding and subtracting semi-opaque
    ;; white layers on top of the image
    (let ((nfade 20)
          (nwait 60)
          (alpha))

      ;; First add the opaque layers to the dictionary
      ;; Increase the rate near the end by increasing alpha as well
      ;; Note: this is still a little rough - a better sequence of alphas would smooth it out.
      (dotimes (id nfade)
        (setf alpha (+ 25 id))
        (add-to-movie m (make-instance 'tag-define-shape :id (+ 4 id)
                                       :shape (draw-shape-with-style `(,(make-rect-contour (getf img1 :width) (getf img1 :height)))
                                               :stroke-width 1
                                               :stroke-color a-white
                                               :fill-color (make-instance 'rgba :r 255 :g 255 :b 255 :a alpha)))))

      ;; Add the image to the movie
      (add-to-movie m (make-instance 'tag-place-object2 :id 3 :depth 3))

      ;; Add all the translucent layers on top of it
      (dotimes (id nfade)
        (add-to-movie m (make-instance 'tag-place-object2 :id (+ 4 id) :depth (+ 4 id))))

      ;; Show what we've got (should be invisible at this point)
      (add-to-movie m (make-instance 'tag-showframe))

      ;; Remove the layers one at a time
      (dotimes (id nfade)
        (add-to-movie m (make-instance 'tag-remove-object :depth (+ 4 id)))
        (add-to-movie m (make-instance 'tag-showframe)))

      ;; Wait awhile
      (dotimes (id nwait)
        (add-to-movie m (make-instance 'tag-showframe)))

      ;; Now put translucent layers back again
      (dotimes (id nfade)
        (add-to-movie m (make-instance 'tag-place-object2 :id (+ 4 id) :depth (+ 4 id)))
        (add-to-movie m (make-instance 'tag-showframe))))

    (add-to-movie m (make-instance 'tag-end))))
