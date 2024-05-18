;;;; package.lisp

(defpackage #:kimpy
  (:import-from :alexandria #:switch)
  (:import-from :iterate #:iter)
  (:use #:cl #:sketch)
  (:local-nicknames (:jzon :com.inuoe.jzon)))

;; (in-package #:kimpy)
