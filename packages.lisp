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

(defpackage #:gordon
  (:use #:cl)
  (:export
   #:*blue* 
   #:*black* 
   #:*brown*
   #:*cyan*
   #:*green*
   #:*grey*
   #:*lblue*
   #:*lcyan*
   #:*lgreen*
   #:*lgrey*
   #:*lmagenta*
   #:*lred*
   #:*magenta*
   #:*red*
   #:*white*
   #:*yellow*
   #:curved-edge-record
   #:straight-edge-record
   #:style-change-record
   #:shape
   #:tag-define-font-2
   #:gradient
   #:rect
   #:matrix
   #:gradient-record
   #:*a-black*
   #:*a-blue*
   #:*a-brown*
   #:*a-cyan*
   #:*a-green*
   #:*a-grey*
   #:*a-lblue*
   #:*a-lcyan*
   #:*a-lgreen*
   #:*a-lgrey*
   #:*a-lmagenta*
   #:*a-lred*
   #:*a-magenta*
   #:*a-red*
   #:*a-white*
   #:*a-yellow*
   #:with-movie
   #:invert-contour-vertical
   #:final-point
   #:curved-edge-p
   #:straight-edge-p
   #:*repeating-bitmap-fill*
   #:*clipped-bitmap-fill*
   #:make-rotation-matrix-angle
   #:add-to-movie
   #:add-shape

   #:*-matrix
   #:+-matrix
   #:flat-marshall-data
   #:marshall
   #:make-records-from-contour
   #:make-shape
   #:tag-define-font-2
   #:make-jpeg-tag
   #:make-style-change-record
   #:make-straight-edge-record
   #:make-curved-edge-record
   #:make-bridge-contour
   #:rgba
   #:rgb
   #:rgb-r
   #:rgb-b
   #:rgb-g
   #:rgba-a
   #:cxform-wa
   #:tag-define-shape
   #:tag-place-object2
   #:random-argb
   #:random-vector
   #:translation-matrix
   #:xrandom
   #:tag-define-edit-text
   #:close-contour
   #:scale-contours
   #:tag-SBC
   #:write-movie

   #:tag-end
   #:tag-enable-debugger2
   #:tag-remove-object
   #:draw-shape-with-style
   #:make-action-record-get-url
   #:make-action-record-goto-frame
   #:make-action-record-goto-label
   #:make-action-record-next-frame
   #:make-action-record-play
   #:make-action-record-previous-frame
   #:make-action-record-push
   #:make-action-record-set-target
   #:make-action-record-set-variable
   #:make-action-record-stop
   #:make-action-record-stop-sounds
   #:make-action-record-toggle-quality
   #:make-action-record-wait-for-frame
   #:tag-do-action
   #:tag-showframe
   #:make-polygone-contour
   #:make-circle-contour
   #:make-rect-contour
   #:tag-define-bits3
   #:button-record
   #:tag-define-button-2

   ))
  
