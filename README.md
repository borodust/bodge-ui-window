# bodge-ui-window

Utility to quickly setup GUI for your Common Lisp application. It's an integration of several
technologies: `bodge-host` for managing OS resources and input, `bodge-ui` for handling UI and
`bodge-canvas` for drawing.

## Example

Have a look at [basic example](examples/basic-example.org) source.

## Install

```lisp
;; Add cl-bodge distribution into quicklisp
(ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.testing.txt" :replace t :prompt nil)

;; Update main dist just in case
(ql:update-dist "quicklisp")

;; Uncomment and evaluate next line only if you wish to enable OpenGL 2 renderer
;; (cl:pushnew :bodge-gl2 cl:*features*)

;; Load the example
(ql:quickload :bodge-ui-window/examples)
;; And run it!
(bodge-ui-window.example.basic:run)
```
