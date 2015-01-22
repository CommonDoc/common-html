(in-package :cl-user)
(defpackage common-html-test
  (:use :cl :fiveam :common-doc)
  (:import-from :common-doc.util
                :doc))
(in-package :common-html-test)

;;; Utils

(defmacro emit-equal (node string)
  `(is
    (equal (common-html.emitter:node-to-html-string ,node)
           ,string)))

(defun mk-text (string)
  (doc text-node (:text string)))

(defun mk-text-item (string)
  (doc list-item () (text-node (:text string))))

;;; Tests

(def-suite tests
  :description "CommonHtml tests.")
(in-suite tests)

(test text
  (emit-equal (mk-text "test") "test"))

(test paragraph
  (emit-equal (doc
               paragraph
               ()
               (text-node
                (:text "test")))
              "<p>test</p>"))

(test markup
  (emit-equal (doc
               bold
               ()
               (text-node
                (:text "test")))
              "<b>test</b>")
  (emit-equal (doc
               italic
               ()
               (text-node
                (:text "test")))
              "<i>test</i>")
  (emit-equal (doc
               underline
               ()
               (text-node
                (:text "test")))
              "<u>test</u>")
  (emit-equal (doc
               strikethrough
               ()
               (text-node
                (:text "test")))
              "<strike>test</strike>")
  (emit-equal (doc
               code
               ()
               (text-node
                (:text "test")))
              "<code>test</code>")
  (emit-equal (doc
               superscript
               ()
               (text-node
                (:text "test")))
              "<sup>test</sup>")
  (emit-equal (doc
               subscript
               ()
               (text-node
                (:text "test")))
              "<sub>test</sub>")
  (emit-equal (doc
               bold
               ()
               (italic
                ()
                (underline
                 ()
                 (text-node
                  (:text "test")))))
              "<b><i><u>test</u></i></b>"))

(test link
  (let ((uri "http://example.com/"))
    (emit-equal (doc
                 web-link
                 (:uri (quri:uri uri))
                 (text-node
                  (:text "test")))
                (format nil "<a href=\"~A\">test</a>" uri))))

(test list
  (emit-equal (doc
               unordered-list
               (:children (list
                           (mk-text-item "1")
                           (mk-text-item "2")
                           (mk-text-item "3"))))
              "<ul><li>1</li><li>2</li><li>3</li></ul>")
  (emit-equal (doc
               ordered-list
               (:children (list
                           (mk-text-item "1")
                           (mk-text-item "2")
                           (mk-text-item "3"))))
              "<ol><li>1</li><li>2</li><li>3</li></ol>")
  (emit-equal (doc
               definition-list
               (:children (list
                           (doc
                            definition
                            (:term (mk-text "a")
                             :definition (mk-text "1")))
                           (doc
                            definition
                            (:term (mk-text "b")
                             :definition (mk-text "2")))
                           (doc
                            definition
                            (:term (mk-text "c")
                             :definition (mk-text "3"))))))
              "<dl><dt>a</dt><dd>1</dd><dt>b</dt><dd>2</dd><dt>c</dt><dd>3</dd></dl>"))

(test image
  (let* ((src "fig.jpg")
         (desc "desc")
         (document
          (doc
           image
           (:source src
            :description desc))))
    (emit-equal document
                (format nil "<img src=~S alt=~S title=~S/>" src desc desc))))

(test figure
  (let* ((src "fig.jpg")
         (desc "desc")
         (figdesc "description")
         (document
          (doc
           figure
           (:image (doc
                    image
                    (:source src
                     :description desc))
            :description (mk-text figdesc)))))
    (emit-equal document
                (format nil
                        "<figure><img src=~S alt=~S title=~S/><figcaption>~A</figcaption></figure>"
                        src desc desc figdesc))))

(test table
  (let* ((matrix
           '((1 2 3)
             (4 5 6)
             (7 8 9)))
         (document
           (doc
            table
            (:rows
             (loop for row in matrix collecting
               (doc
                row
                (:cells
                 (loop for n in row collecting
                   (doc cell () (text-node (:text (write-to-string n))))))))))))
    (emit-equal document
                (format nil "<table>~{<tr>~{<td>~A</td>~}</tr>~}</table>"
                        matrix))))

(test section
  (let ((document
          (doc
           section
           (:title (mk-text "Sec 1"))
           (section
            (:title (mk-text "Sec 1.1"))
            (section
             (:title (mk-text "Sec 1.1.1"))))
           (section
            (:title (mk-text "Sec 1.2"))))))
    (emit-equal document
                (format nil "~{~A~}"
                        (list "<h1>Sec 1</h1>"
                              "<h2>Sec 1.1</h2>"
                              "<h3>Sec 1.1.1</h3>"
                              "<h2>Sec 1.2</h2>")))))

(test document
  (let ((document
          (doc
           document
           (:title "My Title")
           (text-node
            (:text "test")))))
    (emit-equal document
                "<html><head><title>My Title</title></head><body>test</body></html>")))

(run! 'tests)
