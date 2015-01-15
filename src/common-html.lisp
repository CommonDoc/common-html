(in-package :cl-user)
(defpackage common-html
  (:use :cl)
  (:export :html)
  (:documentation "Main interface."))
(in-package :common-html)

(defclass html (common-doc.format:document-format)
  ()
  (:documentation "CommonDoc HTML format class."))

(defmethod common-doc.format:emit-document ((html html)
                                            (doc common-doc:document)
                                            stream)
  (common-html.emitter:node-to-stream doc stream))

(defmethod common-doc.format:emit-document ((html html)
                                            (node common-doc:document-node)
                                            stream)
  (common-html.emitter:node-to-stream node stream))
