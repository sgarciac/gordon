;;; COPYRIGHT 2006 Sergio Garcia <sergio.garcia@gmail.com>
;;; 
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 2.1 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA



(in-package #:gordon)

;;; colors


(defparameter *black* (make-instance 'rgb :r 0 :g 0 :b 0))
(defparameter *blue* (make-instance 'rgb :r 0 :g 0 :b 255))
(defparameter *brown* (make-instance 'rgb :r 128 :g 128 :b 0))
(defparameter *cyan* (make-instance 'rgb :r 0 :g 0 :b 128))
(defparameter *green* (make-instance 'rgb :r 0 :g 128 :b 0))
(defparameter *grey* (make-instance 'rgb :r 128 :g 128 :b 128))
(defparameter *lblue* (make-instance 'rgb :r 0 :g 255 :b 255))
(defparameter *lcyan* (make-instance 'rgb :r 0 :g 128 :b 128))
(defparameter *lgreen* (make-instance 'rgb :r 0 :g 255 :b 0))
(defparameter *lgrey* (make-instance 'rgb :r 192 :g 192 :b 192))
(defparameter *lmagenta* (make-instance 'rgb :r 255 :g 0 :b 255))
(defparameter *lred* (make-instance 'rgb :r 255 :g 0 :b 0))
(defparameter *magenta* (make-instance 'rgb :r 128 :g 0 :b 128))
(defparameter *red* (make-instance 'rgb :r 255 :g 0 :b 0))
(defparameter *white* (make-instance 'rgb :r 255 :g 255 :b 255))
(defparameter *yellow* (make-instance 'rgb :r 255 :g 255 :b 0))

(defparameter *a-black* (make-instance 'rgba :r 0 :g 0 :b 0 :a 255))
(defparameter *a-blue* (make-instance 'rgba :r 0 :g 0 :b 255 :a 255))
(defparameter *a-brown* (make-instance 'rgba :r 128 :g 128 :b 0 :a 255))
(defparameter *a-cyan* (make-instance 'rgba :r 0 :g 0 :b 128 :a 255))
(defparameter *a-green* (make-instance 'rgba :r 0 :g 128 :b 0 :a 255))
(defparameter *a-grey* (make-instance 'rgba :r 128 :g 128 :b 128 :a 255))
(defparameter *a-lblue* (make-instance 'rgba :r 0 :g 255 :b 255 :a 255))
(defparameter *a-lcyan* (make-instance 'rgba :r 0 :g 128 :b 128 :a 255))
(defparameter *a-lgreen* (make-instance 'rgba :r 0 :g 255 :b 0 :a 255))
(defparameter *a-lgrey* (make-instance 'rgba :r 192 :g 192 :b 192 :a 255))
(defparameter *a-lmagenta* (make-instance 'rgba :r 255 :g 0 :b 255 :a 255))
(defparameter *a-lred* (make-instance 'rgba :r 255 :g 0 :b 0 :a 255))
(defparameter *a-magenta* (make-instance 'rgba :r 128 :g 0 :b 128 :a 255))
(defparameter *a-red* (make-instance 'rgba :r 255 :g 0 :b 0 :a 255))
(defparameter *a-white* (make-instance 'rgba :r 255 :g 255 :b 255 :a 255))
(defparameter *a-yellow* (make-instance 'rgba :r 255 :g 255 :b 0 :a 255))
