(in-package :cl-user)
(defpackage common-html.multi-emit
  (:use :cl :common-doc)
  (:export :multi-emit))
(in-package :common-html.multi-emit)

(defun extract-section-id-table (document)
  "Create a hash table that maps the position of a section (0 for first, 1 for
second, etc.) to a unique section ID."
  (let ((table (make-hash-table))
        (current-pos 0))
    (labels ((slug-in-table-p (slug)
               (member slug
                       (alexandria:hash-table-values table)
                       :test #'equal))
             (add-section-id (section)
               ;; Extract a slug from a section's title, and add it to the
               ;; table, modifying it if it's already there.
               (let* ((section-text (common-doc.ops:collect-all-text section))
                      (section-slug (common-doc.util:string-to-slug section-text)))
                 (setf (gethash current-pos table)
                       (if (slug-in-table-p section-slug)
                           (concatenate 'string
                                        (write-to-string current-pos)
                                        "-"
                                        section-slug)
                           section-slug))
                 (incf current-pos))))
      (common-doc.ops:with-document-traversal (document node)
        (when (typep node 'section)
          (add-section-id node)))
      table)))

(defmethod multi-emit ((doc document) directory)
  (let ((table (extract-section-id-table doc))
        (section-pos 0))
    (labels ((process-section (section depth)
               (let ((ordinary-nodes (list))
                     (sub-sections (list)))
                 (loop for child in (children section) do
                   (if (typep child 'section)
                       (push child sub-sections)
                       (push child ordinary-nodes)))
                 (let* ((section-slug (gethash section-pos table))
                        (output-filename (make-pathname :name section-slug
                                                        :type "html"
                                                        :directory directory))
                        (section-content (make-instance 'content-node
                                                        :children (reverse ordinary-nodes))))
                   ;; Here, we emit the section content into the file
                   (let* ((content-string
                            (common-html.emitter:node-to-html-string section-content))
                          (html
                            (common-html.template:template-section doc
                                                                   (title section)
                                                                   (reference section)
                                                                   content-string)))
                     (with-open-file (output-stream output-filename
                                                    :direction :output
                                                    :if-exists :supersede)
                       (write-string html output-stream)))
                   ;; We increase the section position by one
                   (incf section-pos)
                   ;; And finally, go through the subsections, processing each
                   ;; at a time
                   (loop for sub-sec in sub-sections do
                     (process-section sub-sec (1+ depth)))))))
      (loop for child in (children doc) do
        (if (typep child 'section)
            (process-section child 0))))))
