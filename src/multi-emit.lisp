(in-package :cl-user)
(defpackage common-html.multi-emit
  (:use :cl :common-doc)
  (:export :multi-emit))
(in-package :common-html.multi-emit)

(defmethod process-section ((section section) (doc document))
  (print (text (title section)))
  (loop for child in (children section) do
    (if (typep child 'section)
        (process-section child doc))))

(defmethod multi-emit ((doc document))
  (loop for child in (children doc) do
    (if (typep child 'section)
        (process-section child doc))))
