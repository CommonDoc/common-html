(in-package :common-html.multi-emit)

(defvar *multi-emit* nil
  "Whether we are in multi-file emission or not. nil by default.")

(defvar *section-tree* (make-hash-table :test #'equal)
  "A map of section references to parent section references.")

(defvar *current-section-id*)

(defun assign-container (section container)
  (setf (gethash (reference section) *section-tree*)
        container)
  (loop for child in (children section) do
    (if (typep child 'section)
        (assign-container child container))))

(defmethod emit-node ((node document-node) &key directory doc depth max-depth)
  "Emit a document node."
  (declare (ignore directory doc depth max-depth))
  t)

(defmethod emit-node ((node content-node) &key directory doc depth max-depth)
  "Emit a content node."
  (loop for child in (children node) do
    (emit-node child
               :directory directory
               :doc doc
               :depth depth
               :max-depth max-depth)))

(defmethod emit-node ((section section) &key directory doc depth max-depth)
  "Emit a section node."
  (let ((ordinary-nodes (list))
        (sub-sections (list))
        (section-ref (reference section)))
    ;; Add the section ID to the location map
    (setf (gethash section-ref *section-tree*) t)
    ;; Go through the children
    (loop for child in (children section) do
      (if (typep child 'section)
          (if (and max-depth (>= depth (1- max-depth)))
              (push child ordinary-nodes)
              (push child sub-sections))
          (push child ordinary-nodes)))
    (loop for child in ordinary-nodes do
      (if (typep child 'section)
          (assign-container child section-ref)))
    (let* ((output-filename (make-pathname :name section-ref
                                           :type "html"
                                           :defaults directory))
           (section-content (make-instance 'content-node
                                           :children (reverse ordinary-nodes))))
      ;; Here, we emit the section content into the file
      (let* ((content-string
               (common-html.emitter:node-to-html-string section-content))
             (html
               (common-html.template:template-section doc
                                                      section
                                                      content-string)))
        (with-open-file (output-stream output-filename
                                       :direction :output
                                       :if-exists :supersede)
          (write-string html output-stream)))
      ;; And finally, go through the subsections, processing each
      ;; at a time
      (loop for sub-sec in (reverse sub-sections) do
        (emit-node sub-sec
                   :directory directory
                   :doc doc
                   :depth (1+ depth)
                   :max-depth max-depth)))))

(defmethod multi-emit ((doc document) directory &key max-depth)
  (common-doc.ops:fill-unique-refs doc)
  (let ((*multi-emit* t)
        (*section-tree* (make-hash-table :test #'equal)))
    (loop for child in (children doc) do
      (emit-node child
                 :directory directory
                 :doc doc
                 :depth 0
                 :max-depth max-depth))))
