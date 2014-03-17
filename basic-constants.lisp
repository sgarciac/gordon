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

(defparameter *flash-version* 6)
(defparameter *solid-fill* 0)
(defparameter *linear-gradient-fill* 16)
(defparameter *radial-gradient-fill* 18)
(defparameter *repeating-bitmap-fill* 64)
(defparameter *clipped-bitmap-fill* 65)
(defparameter *non-smoothed-repeating-bitmap* 66)
(defparameter *non-smoothed-clipped-bitmap* 67)

(defparameter *yes-bit* #*1)
(defparameter *no-bit* #*0)

(defparameter *empty-ba* (make-array 0 :element-type '(unsigned-byte 8)))
(defparameter *empty-bita* (make-array 0 :element-type 'bit))