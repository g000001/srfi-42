;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-42
  (:export))

(defpackage :srfi-42-internal
  (:use :srfi-42 :cl :fiveam :mbe))

