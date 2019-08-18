;;;; alien-ring.lisp

(in-package #:alien-ring)

(defclass ring-buffer ()
  ((read-index :initform 0 :type (unsigned-byte 32))
   (write-index :initform 0 :type (unsigned-byte 32))
   (capacity :initarg :capacity :accessor ring-buffer-capacity)
   (buffer :initform (null-pointer))))

(defmethod initialize-instance :after ((buf ring-buffer) &key)
  (with-slots (buffer capacity) buf
    (assert (< capacity (ash 1 31)))
    (setf buffer (foreign-alloc :uint8 :initial-element 0 :count capacity))))

(defun make-ring-buffer (size)
  (make-instance 'ring-buffer :capacity size))

(defun ring-buffer-alien (ringbuf)
  (slot-value ringbuf 'buffer))

(defun ring-buffer-write-locations (ringbuf n)
  (let ((max-allowed-write-size (min n (ring-buffer-available ringbuf))))
    (cond
      ((> (+ (ring-buffer-wr ringbuf) max-allowed-write-size)
	  (ring-buffer-capacity ringbuf))
       (let ((part1 (- (ring-buffer-capacity ringbuf) (ring-buffer-wr ringbuf))))
	 (list (cons (ring-buffer-wr ringbuf) part1)
	       (cons 0 (- max-allowed-write-size part1)))))
      (t (list (cons (ring-buffer-wr ringbuf)
		     (- (ring-buffer-capacity ringbuf) (ring-buffer-wr ringbuf))))))))

(defun ring-buffer-read-locations (ringbuf &optional (n (ring-buffer-size ringbuf)))
  (let ((max-allowed-read-size (min n (ring-buffer-size ringbuf))))
    (cond
      ((> (+ (ring-buffer-rd ringbuf) max-allowed-read-size) (ring-buffer-capacity ringbuf))
       (let ((part1 (- (ring-buffer-capacity ringbuf) (ring-buffer-rd ringbuf))))
	 (list (cons (ring-buffer-rd ringbuf) part1)
	       (cons 0 (- max-allowed-read-size part1)))))
      (t (list (cons (ring-buffer-rd ringbuf)
		     (- (ring-buffer-size ringbuf) (ring-buffer-rd ringbuf))))))))

(defun ring-buffer-size (ringbuf)
  (with-slots (read-index write-index capacity) ringbuf
    (logand (- write-index read-index) (1- capacity))))

(defun ring-buffer-empty-p (ringbuf)
  (with-slots (read-index write-index) ringbuf
    (= read-index write-index)))

(defun ring-buffer-full (ringbuf)
  (= (ring-buffer-size ringbuf) (slot-value ringbuf 'capacity)))

(defun ring-buffer-available (ringbuf)
  (- (ring-buffer-capacity ringbuf) (ring-buffer-size ringbuf)))

(defun ring-buffer-advance-rd (ringbuf &optional (n 1))
  (with-slots (read-index) ringbuf
    (setf read-index (logand (+ read-index n) #xFFFFFFFF))))

(defun ring-buffer-advance-wr (ringbuf &optional (n 1))
  (with-slots (write-index) ringbuf
    (setf write-index (logand (+ write-index n) #xFFFFFFFF))))

(defun ring-buffer-rd (ringbuf &optional (offset 0))
  (with-slots (read-index capacity) ringbuf
    (logand (+ read-index offset) (1- capacity))))

(defun ring-buffer-wr (ringbuf)
  (with-slots (write-index capacity) ringbuf
    (logand write-index (1- capacity))))

(defun write-single-element (ringbuf element)
  (assert (not (ring-buffer-full ringbuf)))
  (with-slots (buffer) ringbuf
    (setf (mem-aref buffer :uint8 (ring-buffer-wr ringbuf)) element)
    (ring-buffer-advance-wr ringbuf)))

(defun read-single-element (ringbuf)
  "Reads a single element at read-index and advances the read-index by 1."  
  (assert (not (ring-buffer-empty-p ringbuf)))
  (with-slots (buffer) ringbuf
    (let ((elem (mem-aref buffer :uint8 (ring-buffer-rd ringbuf))))
      (ring-buffer-advance-rd ringbuf)
      elem)))

(defun peek-single-element (ringbuf offset)
  "Reads a single element at read-index + optional offset without advancing the read-index."
  (assert (not (ring-buffer-empty-p ringbuf)))
  (with-slots (buffer) ringbuf
    (mem-aref buffer :uint8 (ring-buffer-rd ringbuf offset))))

(defun ring-buffer-write-byte-sequence (ringbuf seq)
  (let ((write-size (min (ring-buffer-available ringbuf) (length seq))))
    (loop for elem across seq
       for i from 0 below write-size
       do
	 (write-single-element ringbuf elem))))

(defun ring-buffer-write-char-sequence (ringbuf seq)
  (ring-buffer-write-byte-sequence ringbuf (map 'simple-vector #'char-code seq)))

(defun ring-buffer-read-char-sequence (ringbuf &optional n)
  (map 'string #'code-char (ring-buffer-read-byte-sequence ringbuf n)))

(defun ring-buffer-read-byte-sequence (ringbuf &optional n)
  (let ((read-size (cond 
		     ((and (integerp n) (>= n 0))
		      (min (ring-buffer-size ringbuf) n))
		     (t (ring-buffer-size ringbuf)))))
    (loop for i from 0 below read-size
       collect (read-single-element ringbuf))))

(defun read-line-cr-or-lf (ringbuf line-ending)
  (let ((navail (ring-buffer-size ringbuf)))
    (loop for i from 0 below navail
       for elem = (code-char (peek-single-element ringbuf i))
       then (code-char (peek-single-element ringbuf i))
       when (eq elem line-ending)
       collect (ring-buffer-read-char-sequence ringbuf i) into line
       until (eq elem line-ending)
       finally
	 (when line
	   (ring-buffer-advance-rd ringbuf 1)
	   (return (car line))))))

(defun read-line-crlf (ringbuf)
  (let ((navail (ring-buffer-size ringbuf)))
    (loop for i from 0 below navail
       for prev = nil then (code-char (peek-single-element ringbuf (1- i)))
       for elem = (code-char (peek-single-element ringbuf i))
       then (code-char (peek-single-element ringbuf i))
       when (and (eq elem #\newline) (and prev (eq prev #\return)))
       collect (ring-buffer-read-char-sequence ringbuf (1- i)) into line
       until (and (eq elem #\newline) (and prev (eq prev #\return)))
       finally
	 (when line
	   (ring-buffer-advance-rd ringbuf 2)
	   (return (car line))))))

(defun ring-buffer-read-line (ringbuf &optional (line-ending #\newline))
  (case line-ending
    ((#\newline #\return) (read-line-cr-or-lf ringbuf line-ending))
    ((:CRLF) (read-line-crlf ringbuf))))

(defun dump-ring-buffer-characters (ringbuf)
  (with-slots (capacity buffer) ringbuf
    (loop for i from 0 below capacity
       do (format t "~a" (code-char (mem-aref buffer :uint8 i))))))

(defun dump-ring-buffer-bytes (ringbuf)
  (with-slots (capacity buffer) ringbuf
    (loop for i from 0 below capacity
       do (format t "~2,0x " (mem-aref buffer :uint8 i)))))
