(in-package :cl-user)
(defpackage common-html-test
  (:use :cl :fiveam :common-doc)
  (:import-from :common-doc.util
                :doc
                :make-text))
(in-package :common-html-test)

;;; Utils

(defmacro emit-equal (node string)
  `(is
    (equal (common-html.emitter:node-to-html-string ,node)
           ,string)))

(defun make-text-item (string)
  (doc list-item () (text-node (:text string))))

;;; Tests

(def-suite tests
  :description "CommonHtml tests.")
(in-suite tests)

(test text
      (emit-equal (make-text "test") "test"))

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
                               (make-text-item "1")
                               (make-text-item "2")
                               (make-text-item "3"))))
                  "<ul><li>1</li><li>2</li><li>3</li></ul>")
      (emit-equal (doc
                   ordered-list
                   (:children (list
                               (make-text-item "1")
                               (make-text-item "2")
                               (make-text-item "3"))))
                  "<ol><li>1</li><li>2</li><li>3</li></ol>")
      (emit-equal (doc
                   definition-list
                   (:children (list
                               (doc
                                definition
                                (:term (list (make-text "a"))
                                 :definition (list (make-text "1"))))
                               (doc
                                definition
                                (:term (list (make-text "b"))
                                 :definition (list (make-text "2"))))
                               (doc
                                definition
                                (:term (list (make-text "c"))
                                 :definition (list (make-text "3")))))))
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
                 :description (list (make-text figdesc))))))
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
               (:title (list (make-text "Sec 1")))
               (section
                (:title (list (make-text "Sec 1.1")))
                (section
                 (:title (list (make-text "Sec 1.1.1")))))
               (section
                (:title (list (make-text "Sec 1.2")))))))
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
                    "<!DOCTYPE html><html><head><title>My Title</title></head><body>test</body></html>")))
(test multi-file-emission
      (let ((doc
              (doc
               document
               (:title "My Document")
               (section
                (:title (list (make-text "Overview")))
                (text-node
                 (:text "text"))
                (section
                 (:title (list (make-text "History")))
                 (text-node
                  (:text "history"))
                 (section
                  (:title (list (make-text "Motivation")))
                  (text-node
                   (:text "sample &")))))
               (section
                (:title (list (make-text "Tutorial")))
                (bold
                 ()
                 (text-node
                  (:text "bold"))))))
            (output-directory
              (asdf:system-relative-pathname :common-html-test #p"html/")))
        (finishes
         (ensure-directories-exist output-directory))
        (finishes
         (common-html.multi-emit:multi-emit doc output-directory))
        (is
         (equal (length (uiop:directory-files output-directory))
                4))
        (macrolet ((test-file (filename content)
                     `(let ((file (merge-pathnames ,filename output-directory)))
                        (is-true
                         (probe-file file))
                        (is
                         (equal (uiop:read-file-string file)
                                ,content)))))
          (test-file
           "overview.html"
           "<!DOCTYPE html><html><head><title>Overview</title></head><body>text</body></html>")
          (test-file
           "history.html"
           "<!DOCTYPE html><html><head><title>History</title></head><body>history</body></html>")
          (test-file
           "motivation.html"
           "<!DOCTYPE html><html><head><title>Motivation</title></head><body>sample &amp;</body></html>")
          (test-file
           "tutorial.html"
           "<!DOCTYPE html><html><head><title>Tutorial</title></head><body><b>bold</b></body></html>")
          (finishes
           (uiop:delete-directory-tree output-directory :validate t))
          ;; Now, we test emission but with a fixed depth
          (finishes
           (ensure-directories-exist output-directory))
          (finishes
           (common-html.multi-emit:multi-emit doc output-directory :max-depth 2))
          (is
           (equal (length (uiop:directory-files output-directory))
                  3))
          (test-file
           "overview.html"
           "<!DOCTYPE html><html><head><title>Overview</title></head><body>text</body></html>")
          (test-file
           "history.html"
           "<!DOCTYPE html><html><head><title>History</title></head><body>history<h1>Motivation</h1>sample &amp;</body></html>")
          (test-file
           "tutorial.html"
           "<!DOCTYPE html><html><head><title>Tutorial</title></head><body><b>bold</b></body></html>")
          (finishes
           (uiop:delete-directory-tree output-directory :validate t)))))

(test toc
  (let ((doc (doc
              document
              ()
              (section
               (:title (list (make-text "Section 1")))
               (content-node
                ()
                (content-node
                 ()
                 (section
                  (:title (list (make-text "Section 1.1")))))))
              (section
               (:title (list (make-text "Section 2")))))))
    (is
     (equal (common-html.toc:single-file-toc doc)
            "<ol class=\"toc\"><li><a href=\"#section-1\">Section 1</a><ol><li><a href=\"#section-1.1\">Section 1.1</a></li></ol></li><li><a href=\"#section-2\">Section 2</a></li></ol>"))
    (is
     (equal (common-html.toc:multi-file-toc doc)
            "<ol class=\"toc\"><li><a href=\"section-1.html\">Section 1</a><ol><li><a href=\"section-1.1.html\">Section 1.1</a></li></ol></li><li><a href=\"section-2.html\">Section 2</a></li></ol>"))))

(run! 'tests)
