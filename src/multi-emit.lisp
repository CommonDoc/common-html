(in-package :common-html.multi-emit)

(defvar *multi-emit-p* nil
  "Whether we are in multi-file emission or not. nil by default.")

(defmethod multi-emit ((doc document) directory &key max-depth)
  (common-doc.ops:fill-unique-refs doc)
  (let ((*multi-emit-p* t))
    (labels ((process-section (section depth)
               (let ((ordinary-nodes (list))
                     (sub-sections (list))
                     (section-ref (reference section)))
                 (loop for child in (children section) do
                   (if (typep child 'section)
                       (if (and max-depth (>= depth (1- max-depth)))
                           (push child ordinary-nodes)
                           (push child sub-sections))
                       (push child ordinary-nodes)))
                 (let* ((output-filename (make-pathname :name section-ref
                                                        :type "html"
                                                        :defaults directory))
                        (section-content (make-instance 'content-node
                                                        :children (reverse ordinary-nodes))))
                   ;; Here, we emit the section content into the file
                   (let* ((content-string
                            (common-html.emitter:node-to-html-string section-content))
                          (section-title (common-doc.ops:collect-all-text (title section)))
                          (html
                            (common-html.template:template-section doc
                                                                   section-title
                                                                   (reference section)
                                                                   content-string)))
                     (with-open-file (output-stream output-filename
                                                    :direction :output
                                                    :if-exists :supersede)
                       (write-string html output-stream)))
                   ;; And finally, go through the subsections, processing each
                   ;; at a time
                   (loop for sub-sec in (reverse sub-sections) do
                     (process-section sub-sec (1+ depth))))))
             (process-toplevel (node)
               (loop for child in (children node) do
                 (cond
                   ((typep child 'section)
                    (process-section child 0))
                   ((typep child 'content-node)
                    (process-toplevel child))))))
      (process-toplevel doc))))
