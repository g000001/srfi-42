;;;; srfi-42.asd

(cl:in-package :asdf)


(defsystem :srfi-42
  :version "20200130"
  :description "SRFI 42 for Common Lisp: Eager Comprehensions"
  :long-description
  "SRFI 42 for Common Lisp: Eager Comprehensions
https://srfi.schemers.org/srfi-42"
  :author "Sebastian Egner"
  :maintainer "CHIBA Masaomi"
  :license "MIT"
  :serial t
  :depends-on (:mbe :fiveam)
  :components ((:file "package")
               (:file "srfi-42")))


(defmethod perform :after ((o load-op)
                           (c (eql (find-system :srfi-42))))
  (let ((name "https://github.com/g000001/srfi-42")
        (nickname :srfi-42))
    (if (and (find-package nickname)
             (not (eq (find-package nickname) (find-package name))))
        (warn "~A: A package with name ~A already exists."
              name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-42))))
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
        (let* ((pkg "https://github.com/g000001/srfi-42#internals")
               (result (funcall (_ :fiveam :run)
                                (_ pkg :srfi-42))))
          (funcall (_ :fiveam :explain!) result)
          (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))


;;; *EOF*
