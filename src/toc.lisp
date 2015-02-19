(in-package :common-html.toc)

(defun toc-to-html-string (toc)
  "Create an HTML representation of a table of contents."
  (labels ((iterator (toc)
             (markup:markup
              (:ul :class "toc-section"
                   (common-html.emitter:node-to-html-string (getf toc :title))
                   (loop for section in (getf toc :children) collecting
                     (iterator section))))))
    (markup:markup
     (:div :class "toc"
           (loop for section in toc collecting
             (iterator section))))))

(defun html-toc (node)
  "Extract the table of contents of a document, and generate an HTML string of
it."
  (let ((toc (common-doc.ops:table-of-contents node)))
    (toc-to-html-string toc)))
