(in-package :cl-user)
(defpackage common-html
  (:use :cl)
  (:export :<html>)
  (:documentation "Main interface."))
(in-package :common-html)

(defclass <html> (common-doc:<format>)
  ()
  (:documentation "CommonDoc HTML format class."))

(defmethod common-doc:emit-document ((html <html>)
                                     (doc common-doc:<document>)
                                     stream)
  (common-html.emitter:node-to-stream doc stream))
