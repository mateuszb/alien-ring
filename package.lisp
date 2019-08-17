;;;; package.lisp

(defpackage #:alien-ring
  (:use #:cl :cffi)
  (:export :ring-buffer-read-line
	   :ring-buffer-read-byte-sequence
	   :ring-buffer-read-char-sequence
	   :ring-buffer-write-byte-sequence
	   :ring-buffer-write-char-sequence
	   :ring-buffer-size
	   :ring-buffer-available
	   :ring-buffer-capacity
	   :ring-buffer-empty-p
	   :make-ring-buffer
	   :dump-ring-buffer-characters
	   :dump-ring-buffer-bytes))
