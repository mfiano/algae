(in-package #:cl-user)

;;;; A Universally Unique Identifier (UUID) decoder and encoder. The encoder
;;;; supports UUID version 4 only.

(defpackage #:algae.uuid
  (:local-nicknames
   (#:rng #:algae.rng)
   (#:u #:golden-utils))
  (:use #:cl)
  (:export
   #:make-uuid
   #:string->uuid
   #:uuid
   #:uuid-variant
   #:uuid-version
   #:uuid->string))

(in-package #:algae.uuid)

(defstruct (uuid
            (:constructor %make-uuid)
            (:predicate nil)
            (:copier nil))
  version
  (variant :rfc-4122)
  (low 0 :type u:ub64)
  (high 0 :type u:ub64))

(u:define-printer (uuid stream :type nil)
  (format stream "~a" (uuid->string uuid)))

(u:fn-> uuid->string (uuid) string)
(defun uuid->string (uuid)
  (declare (optimize speed))
  (macrolet ((%write (string count offset bits word)
               `(setf ,@(loop :for i :below count
                              :collect `(aref ,string ,(+ offset i))
                              :collect `(aref "0123456789ABCDEF"
                                              (ldb (byte 4 ,(- bits (* i 4))) ,word))))))
    (let ((high (uuid-high uuid))
          (low (uuid-low uuid))
          (string (make-string 36 :element-type 'base-char)))
      (locally (declare (optimize (safety 0)))
        (psetf (aref string 8) #\-
               (aref string 13) #\-
               (aref string 18) #\-
               (aref string 23) #\-)
        (%write string 8 0 60 high)
        (%write string 4 9 28 high)
        (%write string 4 14 12 high)
        (%write string 4 19 60 low)
        (%write string 12 24 44 low))
      string)))

(u:fn-> string->uuid ((simple-string 36)) uuid)
(defun string->uuid (string)
  (flet ((parse-variant (bits)
           (cond
             ((not (logbitp 2 bits))
              :reserved/ncs)
             ((not (logbitp 1 bits))
              :rfc-4122)
             ((not (logbitp 0 bits))
              :reserved/microsoft)
             (t
              :reserved/future))))
    (declare (inline parse-variant %make-uuid))
    (let* ((string (remove #\- string))
           (high (parse-integer string :end 16 :radix 16))
           (low (parse-integer string :start 16 :radix 16)))
      (declare (type u:ub64 high low))
      (%make-uuid :version (ldb (byte 4 12) high)
                  :variant (parse-variant (ldb (byte 3 61) low))
                  :low low
                  :high high))))

(u:fn-> make-uuid (&optional symbol) uuid)
(defun make-uuid (&optional generator-id)
  (declare (optimize speed)
           (inline %make-uuid))
  (flet ((%random ()
           (if generator-id
               (dpb (rng:uint generator-id 0 (1- (expt 2 32)) nil)
                    (byte 32 32)
                    (ldb (byte 32 0) (rng:uint generator-id 0 (1- (expt 2 32)) nil)))
               (random (expt 2 64)))))
    (%make-uuid :version 4
                :low (dpb 4 (byte 3 61) (%random))
                :high (dpb 4 (byte 4 12) (%random)))))
