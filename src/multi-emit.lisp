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
      (common-doc.ops:traverse-document document
                                        (lambda (node)
                                          (when (typep node 'section)
                                            (add-section-id node)))))
      table))

(defmethod process-section ((section section) (doc document))
  (loop for child in (children section) do
    (if (typep child 'section)
        (process-section child doc))))

(defmethod multi-emit ((doc document))
  (loop for child in (children doc) do
    (if (typep child 'section)
        (process-section child doc))))
