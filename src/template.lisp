(in-package :cl-user)
(defpackage common-html.template
  (:use :cl)
  (:import-from :common-doc
                :title
                :children)
  (:export :simplest-template
           :with-template
           :template)
  (:documentation "Document templates."))
(in-package :common-html.template)

(defun simplest-template (document children-string)
  "The simplest template function."
  (format nil "<!DOCTYPE html><html><head><title>~A</title></head><body>~A</body></html>"
          (title document)
          children-string))

(defvar *template-function* #'simplest-template
  "The function that will be used to template a document. Takes two arguments:
An instance of the document class and its children (As an HTML string), and
returns the resulting HTML.")

(defmacro with-template ((template) &rest body)
  "Execute `body` with the template function set to `template`."
  `(let ((*template-function* ,template))
     ,@body))

(defun template (document children-string)
  "Template a document using the current template function."
  (funcall *template-function* document children-string))
