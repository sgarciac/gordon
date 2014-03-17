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

(in-package #:cl-user)

(defpackage #:gordon-system
  (:use #:asdf #:cl))

(in-package #:gordon-system)

(defsystem "gordon"
    :depends-on ()
    :components (
					; external packages
		 (:file "packages")
					; basic infrastructure
		 (:file "basic-constants" :depends-on ("packages"))
		 (:file "utilities" :depends-on ("basic-constants"))
					; ttf related files
					;flash files
		 (:file "basic-classes" :depends-on ("utilities"))		 
		 (:file "basic-types" :depends-on ("basic-constants" "utilities"))
		 (:file "types" :depends-on ("basic-types" "basic-classes"))
		 (:file "constants" :depends-on ("types"))
		 (:file "contours" :depends-on ("constants"))
		 (:file "types-library" :depends-on ("contours"))
		 (:file "actions" :depends-on ("constants"))
		 (:file "tags" :depends-on ("constants"))
		 (:file "movie" :depends-on ("constants"))))
