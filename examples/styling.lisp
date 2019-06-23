(cl:defpackage :bodge-ui-window.example.styling
  (:use :cl :bodge-ui :bodge-host)
  (:export #:run))

(cl:in-package :bodge-ui-window.example.styling)

(defparameter *background-style*
  (list :panel-fixed-background (bodge-math:vec4 0.3 0.6 0.6 0.5)))

(defparameter *ok-style*
  (list :button-normal (bodge-math:vec4 0.3 0.5 0.3 1)
        :button-hover (bodge-math:vec4 0.1 0.4 0.1 1)
        :button-active (bodge-math:vec4 0.0 0.3 0.0 1)))

(defparameter *group-style*
  (list :panel-fixed-background (bodge-math:vec4)))


(defpanel (main-panel
           (:title "Hello Styled Bodge UI")
           (:origin 200 200)
           (:width 400) (:height 150)
           (:options :movable :resizable
                     :minimizable :scrollable
                     :closable)
           (:style :from *background-style*
                   :text-color (bodge-math:vec4 0.6 0.6 0.3 1)))
  (styled-group
   :style *group-style*
   :text-padding (bodge-math:vec2 20 0)
   (label :text "Buttons:")
   (horizontal-layout
    (button :label "Cancel"
            :style `(:button-normal ,(bodge-math:vec4 0.6 0.3 0.3 1)
                     :button-hover ,(bodge-math:vec4 0.5 0.2 0.2 1)
                     :button-active ,(bodge-math:vec4 0.5 0.1 0.1 1)))
    (button :label "Apply" :width 80
            :style `(:button-normal ,(bodge-math:vec4 0.3 0.3 0.6 1)
                     :button-hover ,(bodge-math:vec4 0.2 0.2 0.5 1)
                     :button-active ,(bodge-math:vec4 0.1 0.1 0.5 1)))
    (button :label "OK" :expandable nil :width 100
            :style *ok-style*))))

(cl:in-package :bodge-ui-window.example.styling)

(defparameter *window-width* 800)
(defparameter *window-height* 600)

;; Define main window
(defclass main-window (bodge-ui-window:ui-window) ()
  (:default-initargs
   :title "Bodge UI Styling Example"
   :width *window-width*
   :height *window-height*
   :panels '(main-panel)
   :floating t
   :opengl-version #+bodge-gl2 '(2 1)
                   #-bodge-gl2 '(3 3)))

(cl:in-package :bodge-ui-window.example.styling)

(export 'run)
(defun run ()
  (bodge-host:open-window (make-instance 'main-window)))
