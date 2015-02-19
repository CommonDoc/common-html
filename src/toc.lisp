(in-package :common-html.toc)

(defun toc-to-html-string (toc)
  "Create an HTML representation of a table of contents."
  (with-output-to-string (stream)
    (labels ((iterator (toc)
               (write-string "<ul class='toc-section'><span class='section-title'>" stream)
               (write-string (plump:encode-entities
                              (common-html.emitter:node-to-html-string (getf toc :title)))
                             stream)
               (write-string "</span>" stream)
               (loop for section in (getf toc :children) do
                 (write-string "<li>" stream)
                 (iterator section)
                 (write-string "</li>" stream))
               (write-string "</ul>" stream)))
      (write-string "<div class='toc'>" stream)
      (loop for section in toc do
        (iterator section))
      (write-string "</div>" stream))))

(defun html-toc (node)
  "Extract the table of contents of a document, and generate an HTML string of
it."
  (let ((toc (common-doc.ops:table-of-contents node)))
    (toc-to-html-string toc)))
