(cl:defpackage :bodge-ui-window.example.basic
  (:use :cl :bodge-ui :bodge-host))

(cl:in-package :bodge-ui-window.example.basic)

(defpanel (main-panel
            (:title "Hello Bodge UI")
            (:origin 200 50)
            (:width 400) (:height 400)
            (:options :movable :resizable
                      :minimizable :scrollable
                      :closable))
  (label :text "Nested:")
  (horizontal-layout
   (radio-group
    (radio :label "Option 1")
    (radio :label "Option 2" :activated t))
   (vertical-layout
    (check-box :label "Check 1" :width 100)
    (check-box :label "Check 2"))
   (vertical-layout
    (label :text "Awesomely" :align :left)
    (label :text "Stacked" :align :middle)
    (label :text "Labels" :align :right)))
  (label :text "Expand by width:")
  (horizontal-layout
   (button :label "Dynamic")
   (button :label "Min-Width" :width 80)
   (button :label "Fixed-Width" :expandable nil :width 100))
  (label :text "Expand by ratio:")
  (horizontal-layout
   (button :label "1.0" :expand-ratio 1.0)
   (button :label "0.75" :expand-ratio 0.75)
   (button :label "0.5" :expand-ratio 0.5))
  (label :text "Rest:")
  (button :label "Top-Level Button"))

(cl:in-package :bodge-ui-window.example.basic)

(defparameter *window-width* 800)
(defparameter *window-height* 600)

;; Define main window
(defclass main-window (bodge-ui-window:ui-window) ()
  (:default-initargs
   :title "Bodge UI Window Example"
   :width *window-width*
   :height *window-height*
   :panels '(main-panel)
   :floating t
   ;; for better compatibility we are going to use OpenGL 2.1
   ;; by default, version 3.3 is used
   :opengl-version '(2 1)))

(cl:in-package :bodge-ui-window.example.basic)

(export 'run)
(defun run ()
  (bodge-host:open-window (make-instance 'main-window)))
