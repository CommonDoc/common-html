(in-package :cl-user)
(defpackage cd-html-test
  (:use :cl :fiveam))
(in-package :cd-html-test)

(def-suite tests
  :description "cd-html tests.")
(in-suite tests)

(defun emit-equal (node string)
  (equal (cd-html:node-to-html-string node) string))

(test text
  (is-true
   (emit-equal (make-instance 'common-doc:<text-node>
                              :text "test")
               "test")))

(test paragraphs
  (is-true
   (emit-equal (make-instance 'common-doc:<paragraph>
                              :children
                              (list
                               (make-instance 'common-doc:<text-node>
                                              :text "test")))
               "<p>test</p>")))

(run! 'tests)
