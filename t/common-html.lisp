(in-package :cl-user)
(defpackage common-html-test
  (:use :cl :fiveam :common-doc))
(in-package :common-html-test)

;;; Utils

(defmacro emit-equal (node string)
  `(is
    (equal (common-html.emitter:node-to-html-string ,node)
           ,string)))

(defun make-text-item (string)
  (make-list-item (list (make-text string))))

(defmacro test-markup (constructor tag-name)
  `(emit-equal
    (,constructor
     (list
      (make-text "test")))
   ,(format nil "<~A>test</~A>" tag-name tag-name)))

;;; Tests

(def-suite tests
  :description "CommonHtml tests.")
(in-suite tests)

(test text
  (emit-equal (make-text "test")
              "test"))

(test paragraph
  (test-markup make-paragraph "p"))

(test markup
  (test-markup make-bold "b")
  (test-markup make-italic "i")
  (test-markup make-underline "u")
  (test-markup make-strikethrough "strike")
  (test-markup make-code "code")
  (test-markup make-superscript "sup")
  (test-markup make-subscript "sub")
  (emit-equal (make-bold
               (list
                (make-italic
                 (list
                  (make-underline
                   (list
                    (make-text "test")))))))
              "<b><i><u>test</u></i></b>"))

(test link
  (let ((uri "http://example.com/"))
    (emit-equal (make-web-link uri
                               (list
                                (make-text "test")))
                (format nil "<a href=\"~A\">test</a>" uri))))

(test list
  (emit-equal (make-unordered-list
               (list
                (make-text-item "1")
                (make-text-item "2")
                (make-text-item "3")))
              "<ul><li>1</li><li>2</li><li>3</li></ul>")
  (emit-equal (make-ordered-list
               (list
                (make-text-item "1")
                (make-text-item "2")
                (make-text-item "3")))
              "<ol><li>1</li><li>2</li><li>3</li></ol>")
  (emit-equal (make-definition-list
               (list
                (make-definition
                 (list (make-text "a"))
                 (list (make-text "1")))
                (make-definition
                 (list (make-text "b"))
                 (list (make-text "2")))
                (make-definition
                 (list (make-text "c"))
                 (list (make-text "3")))))
              "<dl><dt>a</dt><dd>1</dd><dt>b</dt><dd>2</dd><dt>c</dt><dd>3</dd></dl>"))

(test image
  (let* ((src "fig.jpg")
         (desc "desc")
         (document (make-image src :description desc)))
    (emit-equal document
                (format nil "<img src=~S alt=~S title=~S/>" src desc desc))))

(test figure
  (let* ((src "fig.jpg")
         (desc "desc")
         (figdesc "description")
         (document
           (make-figure
            (make-image src :description desc)
            (list
             (make-text figdesc)))))
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
           (make-table
            (loop for row in matrix collecting
              (make-row
               (loop for n in row collecting
                (make-cell
                 (list (make-text (write-to-string n))))))))))
    (emit-equal document
                (format nil "<table>~{<tr>~{<td>~A</td>~}</tr>~}</table>"
                        matrix))))

(test section
  (let ((document
          (make-section (list (make-text "Sec 1"))
                        :children
                        (list
                         (make-section (list (make-text "Sec 1.1"))
                                       :children
                                       (list
                                        (make-section (list (make-text "Sec 1.1.1")))))
                         (make-section (list (make-text "Sec 1.2")))))))
    (emit-equal document
                (format nil "~{~A~}"
                        (list "<h1>Sec 1</h1>"
                              "<h2>Sec 1.1</h2>"
                              "<h3>Sec 1.1.1</h3>"
                              "<h2>Sec 1.2</h2>")))))

(test document
  (let ((document (make-document "My Title"
                                 :children (list (make-text "test")))))
    (emit-equal document
                "<!DOCTYPE html><html><head><title>My Title</title></head><body>test</body></html>")))

(test multi-file-emission
  (let ((doc
          (make-document
           "My Document"
           :children
           (list
            (make-section (list (make-text "Overview"))
                          :children
                          (list
                           (make-text "text")
                           (make-section (list (make-text "History"))
                                         :children
                                         (list
                                          (make-text "history")
                                          (make-section (list (make-text "Motivation"))
                                                        :children
                                                        (list
                                                         (make-text "sample &")))))))
            (make-section (list (make-text "Tutorial"))
                          :children
                          (list (make-bold (list (make-text "bold"))))))))
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
       "<!DOCTYPE html><html><head><title>History</title></head><body>history<h1 id=\"motivation\">Motivation</h1>sample &amp;</body></html>")
      (test-file
       "tutorial.html"
       "<!DOCTYPE html><html><head><title>Tutorial</title></head><body><b>bold</b></body></html>")
      (finishes
        (uiop:delete-directory-tree output-directory :validate t)))))

(test toc
  (let ((doc (make-content
              (list
               (make-section
                (list (make-text "Section 1"))
                :children
                (list
                 (make-content
                  (list
                   (make-content
                    (list
                     (make-section (list (make-text "Section 1.1")))))))))
               (make-section (list (make-text "Section 2")))))))
    (is
     (equal (common-html.toc:single-file-toc doc)
            "<ol class=\"toc\"><li><a href=\"#section-1\" data-section=\"section-1\">Section 1</a><ol><li><a href=\"#section-1.1\" data-section=\"section-1.1\">Section 1.1</a></li></ol></li><li><a href=\"#section-2\" data-section=\"section-2\">Section 2</a></li></ol>"))
    (is
     (equal (common-html.toc:multi-file-toc doc)
            "<ol class=\"toc\"><li><a href=\"section-1.html\" data-section=\"section-1\">Section 1</a><ol><li><a href=\"section-1.1.html\" data-section=\"section-1.1\">Section 1.1</a></li></ol></li><li><a href=\"section-2.html\" data-section=\"section-2\">Section 2</a></li></ol>"))))

(run! 'tests)
