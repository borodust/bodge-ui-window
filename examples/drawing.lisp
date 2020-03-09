(cl:defpackage :bodge-ui-window.example.drawing
  (:use :cl :bodge-ui :bodge-host :bodge-canvas :bodge-math)
  (:export #:run))

(cl:in-package :bodge-ui-window.example.drawing)


(defvar *active-layer* :layer-1)


(defun layer-updater (new-layer)
  (lambda (panel)
    (declare (ignore panel))
    (setf *active-layer* new-layer)))


(defpanel (main-panel
           (:title "Control Panel")
           (:origin 25 355)
           (:width 200) (:height 220)
           (:options :movable :resizable
                     :minimizable :scrollable
                     :closable))
  (radio-group
   (radio :label "All" :on-click (layer-updater nil))
   (radio :label "Layer 1" :activated t :on-click (layer-updater :layer-1))
   (radio :label "Layer 2" :on-click (layer-updater :layer-2))
   (radio :label "Layer 3" :on-click (layer-updater :layer-3))))

(cl:in-package :bodge-ui-window.example.drawing)

(defparameter *window-width* 800)
(defparameter *window-height* 600)

;; Define main window
(defclass main-window (bodge-ui-window:ui-window) ()
  (:default-initargs
   :title "Bodge 2D Drawing Example"
   :width *window-width*
   :height *window-height*
   :panels '(main-panel)
   :floating t
   :opengl-version #+bodge-gl2 '(2 1)
                   #-bodge-gl2 '(3 3)))

(cl:in-package :bodge-ui-window.example.drawing)


(defun draw-layer-1 ()
  (with-retained-canvas
    (translate-canvas 10 10)
    (loop for i from 0 below 5
          do (draw-rect (vec2 (* i 125) 0) 80 180 :fill-paint (vec4 0.4 0.2 0.2 1)))))


(defun draw-layer-2 ()
  (with-retained-canvas
    (translate-canvas 10 10)
    (loop for i from 0 below 5
          do (draw-rect (vec2 0 (* i 40)) 580 20 :fill-paint (vec4 0.2 0.4 0.2 1)))))


(defun draw-layer-3 ()
  (with-retained-canvas
    (translate-canvas 112 50)
    (loop for i from 0 below 4
          do (loop for j from 0 below 2
                   do (draw-circle (vec2 (* i 125) (* j 100)) 40 :fill-paint (vec4 0.2 0.2 0.4 1))))))


(defun draw-focused (active-layer)
  (let ((defocused-alpha (if active-layer 0.2 1)))
    (flet ((select-alpha (layer)
             (if (eq active-layer layer) 1 defocused-alpha)))
      (with-alpha ((select-alpha :layer-1))
        (draw-layer-1))
      (with-alpha ((select-alpha :layer-2))
        (draw-layer-2))
      (with-alpha ((select-alpha :layer-3))
        (draw-layer-3)))))


(defun draw-all ()
  (with-alpha (1)
    (draw-layer-1))
  (with-alpha (0.75)
    (draw-layer-2))
  (with-alpha (0.5)
    (draw-layer-3)))


(defun draw-board (active-layer)
  (draw-rect (vec2 0 0) 600 200 :fill-paint (vec4 0.3 0.3 0.3 1))
  (if active-layer
      (draw-focused active-layer)
      (draw-all)))


(defmethod bodge-ui-window:on-draw ((this main-window))
  (translate-canvas 100 100)
  (draw-board *active-layer*))

(cl:in-package :bodge-ui-window.example.drawing)

(export 'run)
(defun run ()
  (bodge-host:open-window (make-instance 'main-window)))
