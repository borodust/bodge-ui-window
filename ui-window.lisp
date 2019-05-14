(cl:defpackage :bodge-ui-window
  (:use :cl)
  (:export #:ui-window
           #:add-window-panel
           #:ui-window-canvas
           #:ui-window-context
           #:on-draw
           #:on-ui-ready
           #:within-rendering-thread
           #:push-rendering-task))
(cl:in-package :bodge-ui-window)


(defgeneric on-draw (window)
  (:method (window) (declare (ignore window))))


(defgeneric on-ui-ready (window)
  (:method (window) (declare (ignore window))))


(defclass ui-window (bodge-host:window)
  ((canvas :initform nil :reader ui-window-canvas)
   (ui-context :initform nil :reader ui-window-context)
   (ui-renderer :initform nil)
   (enabled-p :initform t)
   (mouse-actions :initform (list))
   (cursor-position :initform (bodge-math:vec2))
   (characters :initform (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character))
   (keys :initform (make-array 0 :adjustable t :fill-pointer 0 :element-type 'cons))
   (context-queue :initform (bodge-concurrency:make-task-queue))
   (exit-latch :initform (mt:make-latch))
   (panel-classes :initarg :panels :initform nil)
   (background-color :initform (bodge-math:vec4 0.2 0.2 0.2 0.0))
   (framebuffer-size :initform nil)
   (close-on-hiding :initform t :initarg :close-on-hiding)))


(defun push-rendering-task (window task)
  (with-slots (context-queue) window
    (bodge-concurrency:push-task task context-queue)))


(defmacro within-rendering-thread ((window) &body body)
  `(push-rendering-task ,window (lambda () ,@body)))


(defun add-window-panel (window panel-class &rest initargs &key &allow-other-keys)
  (with-slots (ui-context) window
    (apply #'bodge-ui:add-panel ui-context panel-class initargs)))


(defun setup-rendering-context (window)
  (bodge-host:bind-main-rendering-context window)
  (setf (bodge-host:swap-interval) 1)
  (glad:init))


(defun initialize-ui (window window-size pixel-ratio)
  (with-slots (ui-context ui-renderer canvas panel-classes) window
    (setf canvas (bodge-canvas:make-canvas (bodge-math:x window-size)
                                           (bodge-math:y window-size)
                                           :pixel-ratio pixel-ratio)
          ui-renderer (bodge-canvas-ui:make-renderer canvas)
          ui-context (bodge-ui:make-ui ui-renderer :input-source window))
    (loop for panel-init in panel-classes
          for args = (bodge-util:ensure-list panel-init)
          do (apply #'add-window-panel window args))))


(defun release-ui (window)
  (with-slots (ui-context ui-renderer canvas) window
    (bodge-memory:dispose ui-context)
    (bodge-canvas-ui:destroy-renderer ui-renderer)
    (bodge-canvas:destroy-canvas canvas)))


(defun render-ui (window)
  (with-slots (ui-context canvas background-color framebuffer-size) window
    (when background-color
      (bodge-canvas:clear-buffers background-color))
    (bodge-canvas:with-canvas (canvas)
      (bodge-canvas:reset-viewport)
      (on-draw window))
    (bodge-ui:compose-ui ui-context)
    (bodge-host:swap-buffers window)))


(defun run-rendering-loop (window)
  (with-slots (enabled-p context-queue) window
    (tagbody begin
       (restart-case
           (loop while enabled-p
                 do (render-ui window)
                    (bodge-concurrency:drain context-queue))
         (continue-rendering ()
           :report "Restart rendering loop"
           (setf enabled-p t)
           (go begin))
         (stop-rendering ()
           :report "Stop rendering loop"
           (setf enabled-p nil)
           (go end)))
     end)))


(defun start-rendering-thread (window)
  (with-slots (ui-context ui-renderer exit-latch) window
    (let* ((viewport-size (bodge-host:viewport-size window))
           (framebuffer-size (bodge-host:framebuffer-size window))
           (pixel-ratio (/ (bodge-math:x framebuffer-size)
                           (bodge-math:x viewport-size))))
      (bodge-concurrency:in-new-thread ("rendering-thread")
        (unwind-protect
             (progn
               (setup-rendering-context window)
               (initialize-ui window viewport-size pixel-ratio)
               (on-ui-ready window)
               (unwind-protect
                    (run-rendering-loop window)
                 (release-ui window)))
          (mt:open-latch exit-latch))))))


(defmethod bodge-host:on-init :around ((this ui-window))
  (with-slots (ui-context ui-renderer enabled-p framebuffer-size) this
    (setf enabled-p t
          framebuffer-size (bodge-host:framebuffer-size this))
    (start-rendering-thread this))
  (call-next-method))


(defmethod bodge-host:on-destroy :around ((this ui-window))
  (with-slots (enabled-p exit-latch) this
    (setf enabled-p nil)
    (mt:wait-for-latch exit-latch))
  (call-next-method))


(defmethod bodge-host:on-hide :around ((this ui-window))
  (with-slots (close-on-hiding) this
    (unwind-protect
         (call-next-method)
      (when close-on-hiding
        (bodge-host:close-window this)))))


(defmethod bodge-host:on-mouse-action :around ((this ui-window) button action)
  (with-slots (mouse-actions) this
    (alexandria:nconcf mouse-actions (list (cons button action))))
  (call-next-method))


(defmethod bodge-host:on-cursor-movement :around ((this ui-window) x y)
  (with-slots (cursor-position) this
    (setf (bodge-math:x cursor-position) x
          (bodge-math:y cursor-position) y))
  (call-next-method))


(defmethod bodge-host:on-viewport-size-change :around ((this ui-window) width height)
  (with-slots (canvas) this
    (let* ((framebuffer-size (bodge-host:framebuffer-size this))
           (pixel-ratio (/ (bodge-math:x framebuffer-size) width)))
      (within-rendering-thread (this)
        (bodge-canvas:update-canvas-size canvas width height)
        (bodge-canvas:update-canvas-pixel-ratio canvas pixel-ratio)))
    (call-next-method)))


(defmethod bodge-host:on-framebuffer-size-change :around ((this ui-window) width height)
  (with-slots (canvas framebuffer-size) this
    (let* ((viewport-size (bodge-host:viewport-size this))
           (pixel-ratio (/ width (bodge-math:x viewport-size))))
      (within-rendering-thread (this)
        (setf (bodge-math:x framebuffer-size) width
              (bodge-math:y framebuffer-size) height)
        (bodge-canvas:update-canvas-pixel-ratio canvas pixel-ratio)))
    (call-next-method)))


(defmethod bodge-ui:next-mouse-interaction ((this ui-window))
  (with-slots (mouse-actions) this
    (let ((interaction (pop mouse-actions)))
      (values (car interaction) (cdr interaction)))))


(defmethod bodge-ui:last-cursor-position ((this ui-window)
                                          &optional (result-vec2 (bodge-math:vec2)))
  (with-slots (cursor-position) this
    (setf (bodge-math:x result-vec2) (bodge-math:x cursor-position)
          (bodge-math:y result-vec2) (bodge-math:y cursor-position))
    result-vec2))


(defmethod bodge-host:on-character-input ((this ui-window) (character character))
  (with-slots (characters) this
    (vector-push-extend character characters)))


(defmethod bodge-ui:next-character ((this ui-window))
  (with-slots (characters) this
    (unless (alexandria:emptyp characters)
      (vector-pop characters))))


(defmethod bodge-host:on-key-action ((this ui-window) key state)
  (with-slots (keys) this
    (vector-push-extend (cons key state) keys)))


(defmethod bodge-ui:next-keyboard-interaction ((this ui-window))
  (with-slots (keys) this
    (unless (alexandria:emptyp keys)
      (destructuring-bind (key . state) (vector-pop keys)
        (values key state)))))
