(in-package :common-html.toc)

(defun single-file-toc (doc)
  "Generate and HTML table of contents for a single-file document."
  (common-doc.ops:fill-unique-refs doc)
  (common-html.emitter:node-to-html-string (common-doc.ops:table-of-contents doc)))

(defun multi-file-toc (doc)
  "Generate and HTML table of contents for a multi-file document."
  (common-doc.ops:fill-unique-refs doc)
  (let ((common-html.multi-emit:*multi-emit* t))
    (common-html.emitter:node-to-html-string (common-doc.ops:table-of-contents doc))))
