(in-package :cl-user)

(defpackage common-html.template
  (:use :cl)
  (:import-from :common-doc
                :document
                :title
                :children)
  (:export :template
           :render
           :render-section
           :*template*
           :with-template
           :template
           :template-section)
  (:documentation "Document templates."))

(defpackage common-html.multi-emit
  (:use :cl :common-doc)
  (:export :multi-emit
           :*multi-emit-p*))

(defpackage common-html.emitter
  (:use :cl :common-doc)
  (:import-from :common-html.multi-emit
                :*multi-emit-p*)
  (:export :node-to-stream
           :node-to-html-string)
  (:documentation "Emit HTML5 from a CommonDoc document."))

(defpackage common-html.toc
  (:use :cl)
  (:export :single-file-toc
           :multi-file-toc)
  (:documentation "Utilities for building an HTML table of contents."))

(defpackage common-html
  (:use :cl)
  (:export :html)
  (:documentation "Main interface."))
