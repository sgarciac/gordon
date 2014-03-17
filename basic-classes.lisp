;;; COPYRIGHT 2006 Sergio Garcia <sergio.garcia@gmail.com>
;;; COPYRIGHT 2006 Elliott Johnson <ejohnson@linuxmail.org>
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

(defclass swf-movie-object ()()
  (:documentation "A superclass for all flash objects in Gordon."))

(defgeneric marshall (swf-movie-object)
  (:documentation "The generic function which oversees the marshalling of an object."))

(defmethod marshall ((obj swf-movie-object))
  (error "The swf-object class is abstract and therefore cannot marshall body information."))
