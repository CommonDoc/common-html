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
(in-package :common-html.template)

;;; Template classes

(defclass template ()
  ()
  (:documentation "A template."))

(defgeneric render (template document children-string)
  (:documentation "Render a document instance and its children (As an HTML
  string) into an HTML string."))

(defgeneric render-section (template document section-title section-reference
                            content-string)
  (:documentation "Render a section of a document into an HTML string."))

;;; Defaults

(defmethod render ((template template) (document document) children-string)
  "The simplest template."
  (format nil "<!DOCTYPE html><html><head><title>~A</title></head><body>~A</body></html>"
          (title document)
          children-string))

(defmethod render-section ((template template) (document document) section-title
                           section-reference content-string)
  "The simplest section template."
  (declare (ignore section-reference))
  (format nil "<!DOCTYPE html><html><head><title>~A</title></head><body>~A</body></html>"
          section-title
          content-string))

(defvar *template* (make-instance 'template)
  "The template that will be used by template and template-section.")

(defmacro with-template ((template &rest args) &rest body)
  "Execute `body` with the template set to `template`."
  `(let ((*template* (make-instance ,template ,@args))
     ,@body))

(defun template (document children-string)
  "Like render, only using the *template* special variable."
  (render *template* document children-string))

(defun template-section (document section-title section-reference content-string)
  "Like render-section, but uses the *template* special variable."
  (render-section *template* document section-title section-reference
                  content-string))
