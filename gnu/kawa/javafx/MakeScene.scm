(require gnu.kawa.javafx.GroupObjectBuilder)

(module-compile-options warn-undefined-variable: #t
                        warn-invoke-unknown-method: #t)

(define-simple-class MakeScene ()
  (title ::java.lang.String)
  (children ::java.util.List access: 'private)
  (root ::javafx.scene.Parent access: 'private)
  (camera ::javafx.scene.Camera access: 'private)
  (camera-assigned ::boolean access: 'private)
  (cursor ::javafx.scene.Cursor access: 'private)
  (cursor-assigned ::boolean access: 'private)
  (eventDispatcher ::javafx.event.EventDispatcher access: 'private)
  (eventDispatcher-assigned ::boolean access: 'private)
  (width ::double init: -1 access: 'private)
  (height ::double init: -1 access: 'private)
  (depthBuffer ::boolean access: 'private)
  (fill ::javafx.scene.paint.Paint access: 'private)
  (fill-assigned ::boolean access: 'private)
  ((setCamera x::javafx.scene.Camera)::void
   (set! camera x) (set! camera-assigned #t))
  ((setDepthBuffer x::boolean)::void
   (set! depthBuffer x))
  ((setCursor x::javafx.scene.Cursor)::void
   (set! cursor x) (set! cursor-assigned #t))
  ((setEventDispatcher x::javafx.event.EventDispatcher)::void
   (set! eventDispatcher x) (set! eventDispatcher-assigned #t))
  ((setFill x::javafx.scene.paint.Paint)::void
   (set! fill x) (set! fill-assigned #t))
  ((setWidth x::double)::void (set! width x))
  ((setHeight x::double)::void (set! height x))
  ((setRoot r::javafx.scene.Parent)::void
   (if children
       (primitive-throw (java.lang.IllegalArgumentException "setting root after adding children")))
   (set! root r))
  ((getChildren)::java.util.List
   (cond ((not children)
          (if root
              (primitive-throw (java.lang.IllegalArgumentException "adding children after setting root")))
          (set! children (java.util.ArrayList))))
   children)
  ((add n::javafx.scene.Node)
   (cond (children
          (set! root #!null)
          (children:add n))
         (else
           ((getChildren):add n)
           (if (javafx.scene.Parent? n)
               (set! root n)))))
  ((setNodes n::java.util.List)::void
   ((getChildren):addAll n))
  ((build)::javafx.scene.Scene
   (! r (if root root (javafx.scene.Group (getChildren))))
   (! sc (javafx.scene.Scene r width height depthBuffer))
   (if camera-assigned
       (sc:setCamera camera))
   (if fill-assigned
       (sc:setFill fill))
   (if cursor-assigned
       (sc:setCursor cursor))
   (if eventDispatcher-assigned
       (sc:setEventDispatcher eventDispatcher))
   sc))

