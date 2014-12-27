(in-package :cl-user)
(defpackage common-html-test
  (:use :cl :fiveam :common-doc))
(in-package :common-html-test)

(def-suite tests
  :description "CommonHtml tests.")
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
               "<sub>test</sub>"))
  (is-true
   (emit-equal (doc
                <bold>
                ()
                (<italic>
                 ()
                 (<underline>
                  ()
                  (<text-node>
                   (:text "test")))))
               "<b><i><u>test</u></i></b>")))

(test code
  (is-true
   (emit-equal (doc
                <verbatim>
                (:text "1 2 3"))
               "<pre>1 2 3</pre>"))
  (is-true
   (emit-equal (doc
                <code-block>
                (:language "lisp")
                (<verbatim>
                 (:text "1 2 3")))
               "<code language=\"lisp\"><pre>1 2 3</pre></code>")))

(test links
  (let ((uri "http://example.com/"))
    (is-true
     (emit-equal (doc
                  <web-link>
                  (:uri (quri:uri uri))
                  (<text-node>
                   (:text "test")))
                 (format nil "<a href=\"~A\">test</a>" uri)))))

(run! 'tests)
