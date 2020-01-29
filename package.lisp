;;;; package.lisp

(cl:in-package :cl-user)

(defpackage "https://github.com/g000001/srfi-42"
  (:use)
  (:export :fold3-ec
           :fold-ec
           :list-ec
           :do-ec
           :append-ec
           :string-ec
           :string-append-ec
           :vector-ec
           :vector-of-length-ec
           :sum-ec
           :product-ec
           :min-ec
           :max-ec
           :nested
           :last-ec
           :first-ec
           :any?-ec
           :every?-ec
           :index))

(defpackage "https://github.com/g000001/srfi-42#internals"
  (:use "https://github.com/g000001/srfi-42"
        :cl
        :mbe
        :fiveam))

