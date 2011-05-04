;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-42
  (:export :fold3-ec
           :fold-ec
           :list-ec
           :append-ec
           :string-ec
           :string-append-ec
           :vector-ec
           :vector-of-length-ec
           :sum-ec
           :product-ec
           :min-ec
           :max-ec
           :last-ec
           :first-ec
           :any?-ec
           :every?-ec))

(defpackage :srfi-42-internal
  (:use :srfi-42 :cl :fiveam :mbe))

