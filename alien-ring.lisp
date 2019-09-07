;;;; alien-ring.lisp

(in-package #:alien-ring)

(defclass ring-buffer ()
  ((read-index :initform 0 :type (unsigned-byte 32) :accessor read-index)
   (write-index :initform 0 :type (unsigned-byte 32) :accessor write-index)
   (capacity :initarg :capacity :accessor ring-buffer-capacity)
   (buffer :initform (null-pointer) :reader ring-buffer-ptr)))

(defmethod initialize-instance :after ((buf ring-buffer) &key)
  (with-slots (buffer capacity) buf
    (assert (and (< capacity (ash 1 31))
		 (zerop (logand capacity (1- capacity)))))
    (setf buffer (foreign-alloc :uint8 :initial-element 0 :count capacity))))

(defun make-ring-buffer (size)
  (make-instance 'ring-buffer :capacity size))

(defun free-ring-buffer (buf)
  (foreign-free (slot-value buf 'buffer))
  (setf (slot-value buf 'buffer) nil))

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
		     (- max-allowed-write-size (ring-buffer-wr ringbuf))))))))

(defun ring-buffer-read-locations (ringbuf &optional (n (ring-buffer-size ringbuf)))
  (let ((max-allowed-read-size (min n (ring-buffer-size ringbuf))))
    (cond
      ((> (+ (ring-buffer-rd ringbuf) max-allowed-read-size) (ring-buffer-capacity ringbuf))
       (let ((part1 (- (ring-buffer-capacity ringbuf) (ring-buffer-rd ringbuf))))
	 (list (cons (ring-buffer-rd ringbuf) part1)
	       (cons 0 (- max-allowed-read-size part1)))))
      (t
       (list (cons (ring-buffer-rd ringbuf)
		   (ring-buffer-size ringbuf)))))))

(defun ring-buffer-size (ringbuf)
  (with-slots (read-index write-index capacity) ringbuf
    (logand (- write-index read-index) #xFFFFFFFF)))

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
    (ring-buffer-advance-wr ringbuf)
    element))

(defun read-single-element (ringbuf)
  "Reads a single element at read-index and advances the read-index by 1."
  (assert (not (ring-buffer-empty-p ringbuf)))
  (with-slots (buffer) ringbuf
    (let ((elem (mem-aref buffer :uint8 (ring-buffer-rd ringbuf))))
      (ring-buffer-advance-rd ringbuf)
      elem)))

(defun ring-buffer-read-char (ringbuf)
  (unless (zerop (ring-buffer-size ringbuf))
    (code-char (read-single-element ringbuf))))

(defun ring-buffer-read-byte (ringbuf)
  (unless (zerop (ring-buffer-size ringbuf))
    (read-single-element ringbuf)))

(defun ring-buffer-peek-byte (ringbuf &optional (n 0))
  (peek-single-element ringbuf n))

(defun ring-buffer-write-char (ringbuf char)
  (unless (zerop (ring-buffer-available ringbuf))
    (write-single-element ringbuf (char-code char))))

(defun ring-buffer-write-byte (ringbuf byte)
  (unless (zerop (ring-buffer-available ringbuf))
    (write-single-element ringbuf byte)))

(defun peek-single-element (ringbuf &optional (offset 0))
  "Reads a single element at read-index + optional offset without advancing the read-index."
  (when (ring-buffer-empty-p ringbuf)
    nil)
  (with-slots (buffer) ringbuf
    (mem-aref buffer :uint8 (ring-buffer-rd ringbuf offset))))

(defun ring-buffer-peek-char (ringbuf &optional (offset 0))
  "Reads a single char at read-index + optional offset without advancing the read-index."
  (assert (not (ring-buffer-empty-p ringbuf)))
  (with-slots (buffer) ringbuf
    (code-char (mem-aref buffer :uint8 (ring-buffer-rd ringbuf offset)))))

(defun ring-buffer-write-byte-sequence (ringbuf seq &optional (start 0) end)
  (let ((write-size (min
		     (ring-buffer-available ringbuf)
		     (if (and end (> end 0)) (- end start) (length seq)))))
    (loop for elem across seq
       for i from 0 below write-size
       do
	 (write-single-element ringbuf elem))
    write-size))

(defun ring-buffer-write-char-sequence (ringbuf seq)
  (ring-buffer-write-byte-sequence ringbuf (map 'simple-vector #'char-code seq)))

(defun ring-buffer-read-char-sequence (ringbuf &optional n)
  (map 'string #'code-char (ring-buffer-read-byte-sequence ringbuf n)))

(defun ring-buffer-read-byte-sequence (ringbuf &optional n dest)
  (let ((read-size (cond
		     ((and (integerp n) (>= n 0))
		      (min (ring-buffer-size ringbuf) n))
		     (t (ring-buffer-size ringbuf)))))
    (let ((array (if dest dest (make-array read-size :element-type '(unsigned-byte 8)))))
      (loop for i from 0 below read-size
	 do (setf (aref array i) (read-single-element ringbuf)))
      array)))

(defun ring-buffer-peek-byte-sequence (ringbuf &optional n)
  (let ((read-size (cond
		     ((and (integerp n) (>= n 0))
		      (min (ring-buffer-size ringbuf) n))
		     (t (ring-buffer-size ringbuf)))))
    (let ((array (make-array read-size :element-type '(unsigned-byte 8))))
      (loop for i from 0 below read-size
	 do (setf (aref array i) (peek-single-element ringbuf i)))
      array)))

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

(defun ring-buffer-read-token (ringbuf &optional (separator #\space))
  (let ((navail (ring-buffer-size ringbuf)))
    ;; eat separators first
    (loop for i from 0 below navail
       for elem = (code-char (peek-single-element ringbuf))
       then (code-char (peek-single-element ringbuf))
       while (char= elem separator)
       do
	 (read-single-element ringbuf))

    (setf navail (ring-buffer-size ringbuf))
    ;; collect the token
    (loop for i from 0 below navail
       for elem = (code-char (peek-single-element ringbuf))
       then (code-char (peek-single-element ringbuf))
       when (not (char= elem separator))
       collect (ring-buffer-read-char ringbuf) into token
       until (char= elem separator)
       finally
	 (when token
	   (return (map 'string #'identity token))))))

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
       do
	 (format t "~2,'0x " (mem-aref buffer :uint8 i)))
    (terpri)))
