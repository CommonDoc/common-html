(in-package :cl-user)
(defpackage common-html-test
  (:use :cl :fiveam)
  (:import-from :common-doc
                :doc
                :<text-node>
                :<paragraph>
                :<bold>
                :<italic>
                :<underline>
                :<strikethrough>
                :<code>
                :<superscript>
                :<subscript>))
(in-package :common-html-test)

(def-suite tests
  :description "cd-html tests.")
(in-suite tests)

(defun emit-equal (node string)
  (equal (common-html.emitter:node-to-html-string node) string))

(test text
  (is-true
   (emit-equal (doc <text-node> (:text "test")) "test")))

(test paragraphs
  (is-true
   (emit-equal (doc
                <paragraph>
                ()
                (<text-node>
                 (:text "test")))
               "<p>test</p>")))

(test markup
  (is-true
   (emit-equal (doc
                <bold>
                ()
                (<text-node>
                 (:text "test")))
               "<b>test</b>"))
  (is-true
   (emit-equal (doc
                <italic>
                ()
                (<text-node>
                 (:text "test")))
               "<i>test</i>"))
  (is-true
   (emit-equal (doc
                <underline>
                ()
                (<text-node>
                 (:text "test")))
               "<u>test</u>"))
  (is-true
   (emit-equal (doc
                <strikethrough>
                ()
                (<text-node>
                 (:text "test")))
               "<strike>test</strike>"))
  (is-true
   (emit-equal (doc
                <code>
                ()
                (<text-node>
                 (:text "test")))
               "<code>test</code>"))
  (is-true
   (emit-equal (doc
                <superscript>
                ()
                (<text-node>
                 (:text "test")))
               "<sup>test</sup>"))
  (is-true
   (emit-equal (doc
                <subscript>
                ()
                (<text-node>
                 (:text "test")))
               "<sub>test</sub>")))

(run! 'tests)
