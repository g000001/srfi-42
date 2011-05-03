;;;; srfi-42.asd

(cl:in-package :asdf)

(defsystem :srfi-42
  :serial t
  :depends-on (:mbe)
  :components ((:file "package")
               (:file "srfi-42")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-42))))
  (load-system :srfi-42)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-42-internal :srfi-42))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

