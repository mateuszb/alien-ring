(in-package :alien-ring)

(defclass binary-ring-stream (fundamental-binary-output-stream
			      fundamental-binary-input-stream
			      fundamental-character-output-stream
			      fundamental-character-input-stream)
  ((buffer :initform nil :initarg :buffer :reader stream-buffer)))

(defmethod stream-element-type ((stream binary-ring-stream))
  '(unsigned-byte 8))

(defmethod close ((stream binary-ring-stream) &key abort)
  (declare (ignore abort))
  (free-ring-buffer (stream-buffer stream))
  (values))

(defmethod stream-file-position ((stream binary-ring-stream))
  (values (read-index (stream-buffer stream))
	  (write-index (stream-buffer stream))))

(defmethod (setf stream-file-position) ((stream binary-ring-stream) position-spec)
  (let ((ringbuf (stream-buffer stream)))
    (setf (read-index ringbuf) (car position-spec)
	  (write-index ringbuf) (cdr position-spec))
    t))

(defmethod stream-write-sequence ((stream binary-ring-stream) seq start end &key)
  (declare (ignorable start end))
  (ring-buffer-write-byte-sequence (stream-buffer stream) seq start end)
  seq)

(defmethod stream-read-sequence ((stream binary-ring-stream) seq start end &key)
  (declare (ignorable start end))
  (ring-buffer-read-byte-sequence (stream-buffer stream) (length seq) seq)
  (length seq))

(defmethod stream-write-string ((stream binary-ring-stream) s &optional (start 0) (end (length s)))
  (stream-write-sequence stream (map 'simple-vector #'char-code s) start end)
  s)

(defmethod stream-read-byte ((stream binary-ring-stream))
  (ring-buffer-read-byte (stream-buffer stream)))

(defmethod stream-peek-byte ((stream binary-ring-stream) &optional (n 0))
  (ring-buffer-peek-byte (stream-buffer stream) n))

(defmethod stream-peek-char ((stream binary-ring-stream) &optional (n 0))
  (code-char
   (ring-buffer-peek-byte (stream-buffer stream) n)))

(defmethod stream-write-byte ((stream binary-ring-stream) integer)
  (ring-buffer-write-byte (stream-buffer stream) integer))

(defmethod stream-write-char ((stream binary-ring-stream) (character character))
  (ring-buffer-write-byte (stream-buffer stream) (char-code character)))

(defgeneric stream-size (stream))
(defmethod stream-size ((stream binary-ring-stream))
  (ring-buffer-size (stream-buffer stream)))

(defgeneric stream-space-available (stream))
(defmethod stream-space-available ((stream binary-ring-stream))
  (ring-buffer-available (stream-buffer stream)))

(defun make-binary-ring-stream (size)
  (let ((buf (make-ring-buffer size)))
    (make-instance 'binary-ring-stream :buffer buf)))

(defmacro with-output-to-byte-sequence ((var size) &body body)
  (let ((resultvar (gensym)))
    `(let ((,var (make-instance
		  'binary-ring-stream
		  :buffer (make-ring-buffer (expt 2 (ceiling (log ,size 2)))))))
       ,@body
       (let ((,resultvar (make-array ,size :element-type '(unsigned-byte 8))))
	 (read-sequence ,resultvar ,var :end ,size)
	 (close ,var)
	 ,resultvar))))
