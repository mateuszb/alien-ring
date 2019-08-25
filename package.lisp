;;;; package.lisp

(defpackage #:alien-ring
  (:use #:cl :cffi)

  (:import-from :trivial-gray-streams
		:fundamental-binary-output-stream
		:fundamental-binary-input-stream
		:stream-write-byte
		:stream-read-byte)
  
  (:export :ring-buffer-read-line
	   :ring-buffer-read-byte-sequence
	   :ring-buffer-read-char-sequence
	   :ring-buffer-write-byte-sequence
	   :ring-buffer-write-char-sequence
	   :ring-buffer-peek-byte-sequence
	   :ring-buffer-size
	   :ring-buffer-available
	   :ring-buffer-capacity
	   :ring-buffer-empty-p
	   :ring-buffer-alien
	   :ring-buffer-write-locations
	   :ring-buffer-read-char
	   :ring-buffer-read-byte
	   :ring-buffer-write-char
	   :ring-buffer-write-byte
	   :ring-buffer-read-token
	   :ring-buffer-peek-char
	   :make-ring-buffer
	   :dump-ring-buffer-characters
	   :dump-ring-buffer-bytes
	   :stream-read-byte
	   :stream-write-byte
	   :make-binary-ring-stream
	   :stream-space-available
	   :stream-size
	   :stream-read-sequence
	   :stream-write-sequence
	   :stream-file-position
	   :close
	   :stream-element-type
	   :read-index
	   :write-index
	   :stream-peek-byte))
