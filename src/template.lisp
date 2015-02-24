(in-package :common-html.template)

;;; Template classes

(defclass template ()
  ()
  (:documentation "A template."))

(defgeneric render (template document content-string)
  (:documentation "Render a document instance and its children (As an HTML
  string) into an HTML string."))

(defgeneric render-section (template document section content-string)
  (:documentation "Render a section of a document into an HTML string."))

;;; Defaults

(defmethod render ((template template) (document document) content-string)
  "The simplest template."
  (format nil
          "<!DOCTYPE html><html><head><title>~A</title></head><body>~A</body></html>"
          (plump:encode-entities (title document))
          content-string))

(defmethod render-section ((template template) (document document) (section section)
                           content-string)
  "The simplest section template."
  (format nil
          "<!DOCTYPE html><html><head><title>~A</title></head><body>~A</body></html>"
          (plump:encode-entities (common-doc.ops:collect-all-text (title section)))
          content-string))

(defvar *template* (make-instance 'template)
  "The template that will be used by template and template-section.")

(defmacro with-template ((template &rest args) &rest body)
  "Execute `body` with the template set to `template`."
  `(let ((*template* (make-instance ,template ,@args)))
     ,@body))

(defun template (document children-string)
  "Like render, only using the *template* special variable."
  (render *template* document children-string))

(defun template-section (document section content-string)
  "Like render-section, but uses the *template* special variable."
  (render-section *template* document section content-string))
