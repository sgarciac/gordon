;;; Here a collection of marshalling tests are setup to allow 
;;; the accuracy of marshalling code changes to be tested.

(in-package :gordon)

(defun report (object-symbol value)
  (format t "~%~A: ~A" object-symbol value))

(defmacro test-marshalling (object 
                            form 
                            expected-value 
                            test)
  `(report ',object (funcall 
                     ,test 
                     (marshall ,form :output nil)
                     ',expected-value)))

(format t "~%Testing object marshalling from types.lisp")
(test-marshalling rect (make-instance 'rect) (#*000010000) #'equalp)
(test-marshalling rgb (make-instance 'rgb) (#(0) #(0) #(0)) #'equalp)
(test-marshalling rgba (make-instance 'rgba) (#(0) #(0) #(0) #(0)) #'equalp)
(test-marshalling cxform-wa (make-instance 'cxform-wa) (#*0 #*0) #'equal)
(test-marshalling cxform-wa (make-instance 'cxform-wa 
                                           :rmult 2.0 
                                           :gmult 1.0 
                                           :bmult 3.0 
                                           :amult 3.0
                                           :gadd 3 
                                           :radd 1 
                                           :badd 3 
                                           :aadd 2)
                  (#*1 #*1 #*10110100000000000100000000011000000000110000000000000000001000000000110000000001100000000010)
                  #'equal)
(test-marshalling matrix (make-instance 'matrix) (#*0 #*0 #*0000100) #'equal)
(test-marshalling matrix (make-instance 'matrix
                                        :scale-x 2.0 
                                        :scale-y -2.0
                                        :rotate-skew-0 0.3
                                        :rotate-skew-1 -4.5
                                        :translate-x 23
                                        :translate-y -2)
                  (#*1 #*1001101000000000000000001100000000000000000 #*1
                       #*1001100001001100110011000111000000000000000 #*00110010111111110)
                  #'equal)

(test-marshalling gradient-record (make-instance 'gradient-record) (#(1) #(0) #(0) #(0)) #'equalp)
(test-marshalling gradient (make-instance 'gradient) (#(0)) #'equalp)
(test-marshalling gradient (make-instance 'gradient
                                          :record-list 
                                          (loop for i from 0 to 10 collect 
                                                (make-instance 
                                                 'gradient-record)))
                  (#(11) #(1) #(0) #(0) #(0) #(1) #(0) #(0) 
                    #(0) #(1) #(0) #(0) #(0) #(1) #(0) #(0) 
                    #(0) #(1) #(0) #(0) #(0) #(1) #(0) #(0) 
                    #(0) #(1) #(0) #(0) #(0) #(1) #(0) #(0) 
                    #(0) #(1) #(0) #(0) #(0) #(1) #(0) #(0) 
                    #(0) #(1) #(0) #(0) #(0))
                  #'equalp)
(test-marshalling fillstyle 
                  (make-instance 'fillstyle) 
                  (#(0) #* #() #* #())
                  #'equalp)
(test-marshalling fillstyle 
                  (make-instance 'fillstyle
                                 :color (make-instance 'rgb 
                                                       :r 255 
                                                       :g 0 
                                                       :b 128)) 
                   (#(0) #(255) #(0) #(128) #* #() #* #())
                  #'equalp)
(test-marshalling fillstyle
                  (make-instance 'fillstyle
                                 :type *repeating-bitmap-fill*
                                 :bitmap-id 32
                                 :bitmap-matrix (make-instance 'matrix 
                                                               :scale-x 3.3))
                  (#(64) #() #* #() #(32 0) #*1 #*1001101101001100110011000010000000000000000 #*0 #*0000100 #())
                  #'equalp)
(test-marshalling 
 fillstyle
 (make-instance 'fillstyle
                :type *radial-gradient-fill*
                :gradient-matrix (make-instance 'matrix)
                :gradient (make-instance 'gradient 
                                         :record-list 
                                         `(,(make-instance 'gradient-record))))
 (#(18) #() #*0 #*0 #*0000100 #(1) #(1) #(0) #(0) #(0) #* #())
 #'equalp)
(test-marshalling linestyle (make-instance 'linestyle) (#(1 0)) #'equalp)
(test-marshalling linestyle (make-instance 'linestyle 
                                           :width 2000
                                           :color *a-yellow*)
                  (#(208 7) #(255) #(255) #(0) #(255))
                  #'equalp)
(test-marshalling 
 fillstyle-array
 (make-instance 'fillstyle-array 
                :styles (loop for i from 0 to 10 
                              collecting (make-instance 'fillstyle)))
 (#(11) #(0) #* #() #* #() #(0) #* #() #* #() #(0) #* #() #* #() #(0) #* #() #*
   #() #(0) #* #() #* #() #(0) #* #() #* #() #(0) #* #() #* #() #(0) #* #() #*
   #() #(0) #* #() #* #() #(0) #* #() #* #() #(0) #* #() #* #())
 #'equalp)
(test-marshalling 
 linestyle-array
 (make-instance 'linestyle-array
                :styles (loop for i from 0 to 10
                              collecting (make-instance 'linestyle)))
 (#(11) #(1 0) #(1 0) #(1 0) #(1 0) #(1 0) #(1 0) #(1 0) #(1 0) #(1 0) #(1 0)
   #(1 0))
 #'equalp)
(test-marshalling style-change-record 
                  (make-instance 'style-change-record)
                  (#*0 #*0 #*0 #*0 #*0 #*0)
                  #'equal)
;; I need to add more style-change-record tests in here.
(test-marshalling end-record
                  (make-instance 'end-record)
                  (#*0 #*00000)
                  #'equal)
(test-marshalling curved-edge-record
                  (make-instance 'curved-edge-record)
                  (#*1 #*0 #*1111 #*0 #*0 #*0 #*0)
                  #'equal)
(test-marshalling straight-edge-record
                  (make-instance 'straight-edge-record)
                  (#*1 #*1 #*0000 #*0 #*1 #*00)
                  #'equal)
(test-marshalling 
 shape
 (make-instance 'shape 
                :records (list (make-instance 'straight-edge-record)))
 (#*0001 #*0001 #*1 #*1 #*0000 #*0 #*1 #*00 #*0 #*00000)
 #'equalp)
(test-marshalling 
 shape-with-style
 (make-instance 
  'shape-with-style 
  :records (list (make-instance 'curved-edge-record))
  :linestyles (make-instance 'linestyle-array
                             :styles (list 
                                      (make-instance 
                                       'linestyle 
                                       :width 2)))
  :fillstyles (make-instance 'fillstyle-array
                             :styles (list (make-instance 'fillstyle 
                                                          :color *a-red*))))
 (#(1) #(0) #(255) #(0) #(0) #(255) #* #() #* #() #(1) #(2 0) #*0001 #*0001 #*1
   #*0 #*1111 #*0 #*0 #*0 #*0 #*0 #*00000)
 #'equalp)