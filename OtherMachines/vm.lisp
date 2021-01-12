#!/usr/bin/sbcl --script

(defconstant +memwidth+ 8)
(defconstant +exit+       #x00)
(defconstant +nop+        #x01)
(defconstant +jump+       #x02)
(defconstant +jz_fwd+     #x03)
(defconstant +jz_back+    #x04)
(defconstant +set_sp+     #x05)
(defconstant +get_pc+     #x06)
(defconstant +get_sp+     #x07)
(defconstant +push0+      #x08)
(defconstant +push1+      #x09)
(defconstant +push2+      #x0A)
(defconstant +push4+      #x0B)
(defconstant +push8+      #x0C)
(defconstant +load1+      #x10)
(defconstant +load2+      #x11)
(defconstant +load4+      #x12)
(defconstant +load8+      #x13)
(defconstant +store1+     #x14)
(defconstant +store2+     #x15)
(defconstant +store4+     #x16)
(defconstant +store8+     #x17)
(defconstant +add+        #x20)
(defconstant +mult+       #x21)
(defconstant +div+        #x22)
(defconstant +rem+        #x23)
(defconstant +lt+         #x24)
(defconstant +and+        #x28)
(defconstant +or+         #x29)
(defconstant +not+        #x2A)
(defconstant +xor+        #x2B)
(defconstant +pow2+       #x2C)

(defvar *format* :latex)
(defvar *operations* (make-hash-table))

;;;------------------------------------ Arithmetic Utilities --------------------------------------

(defconstant +2^64+ (expt 2 64))
(defconstant +2^64-1+ (1- +2^64+))

(defun octet (i)
  (byte 8 (* 8 i)))

(defun add (x y)
  (mod (+ x y) +2^64+))

(defun mult (x y)
  (mod (* x y) +2^64+))

(defun quotient (x y)
  (if (/= y 0)
      (mod (truncate x y) +2^64+)
      0))

(defun remainder (x y)
  (if (/= y 0)
      (mod (rem x y) +2^64+)
      0))

(defun lt (x y)
  (if (< x y)
      +2^64-1+
      0))

(defun pow2 (x)
  (if (<= 0 x 63)
      (ash 1 x)
      0))

(defmacro increment (place delta &key (size 64))
  `(setf ,place (mod (+ ,place ,delta) (expt 2 ,size))))

(defmacro decrement (place delta &key (size 64))
  `(setf ,place (mod (- ,place ,delta) (expt 2 ,size))))

;;;---------------------------------------- The Machine -------------------------------------------

(defclass machine ()
  ((memwidth  :accessor machine-memwidth  :initarg :memwidth)
   (groups    :accessor machine-groups    :initarg :groups)
   (opcodes   :accessor machine-opcodes   :initform (make-hash-table))
   (memory    :accessor machine-memory    :initform nil)
   (index     :accessor machine-index     :initform 0)
   (term      :accessor machine-term      :initform nil)
   (pc        :accessor machine-pc        :initform 0)
   (sp        :accessor machine-sp        :initform 0)))

(defmethod initialize-instance :after ((machine machine) &rest initargs)
  (declare (ignore initargs))
  (with-slots (memwidth opgroups opcodes memory sp) machine
    (install-operation-groups machine)
    (let ((size (expt 2 memwidth)))
      (setf sp size)
      (setf memory (make-array size
                               :adjustable t
                               :fill-pointer t
                               :element-type '(unsigned-byte 8)
                               :initial-element 0)))))

(defun make-machine (memwidth &key (groups t))
  (make-instance 'machine :memwidth memwidth :groups groups))

(defmethod install-operation-groups ((machine machine))
  (with-slots (opcodes groups) machine
    (maphash (lambda (op function)
               (when (or (eq groups t)
                         (member (operation-group function) groups))
                 (setf (gethash op opcodes) function)))
             *operations*)))

(defmethod run ((machine machine))
  (with-slots (opcodes memory pc sp term) machine
    (do ()
        (term)
      (print-stack machine)
      (let ((p pc))
        (incf pc)
        (assert (< pc sp) () "PC >= SP")
        (let* ((opcode (aref memory p))
               (function (gethash opcode opcodes)))
          (assert function (opcode) "Opcode ~S is unknown" opcode)
          (funcall function machine))))))

;;;-------------------------------------- Basic Procedures ----------------------------------------

(defmethod push* ((machine machine) x)
  (with-slots (memory sp) machine
    (let ((i 8))
      (dotimes (n 8)
        (decf i)
        (decf sp)
        (setf (aref memory sp) (ldb (octet i) x))))))

(defmethod pop* ((machine machine))
  (with-slots (memory sp) machine
    (let ((x 0)
          (i 0))
      (dotimes (n 8)
        (setf (ldb (octet i) x) (aref memory sp))
        (incf i)
        (incf sp))
      x)))

(defmethod put* ((machine machine) n address x)
  (with-slots (memory) machine
    (dotimes (i n)
      (setf (aref memory (+ address i)) (ldb (octet i) x)))))

(defmethod get* ((machine machine) n address)
  (with-slots (memory) machine
    (let ((x 0))
      (dotimes (i n)
        (setf (ldb (octet i) x) (aref memory (+ address i))))
      x)))

(defmethod fetch ((machine machine) n)
  (with-slots (memory pc) machine
    (let ((x 0))
      (dotimes (i n)
        (setf (ldb (octet i) x) (aref memory pc))
        (incf pc))
      x)))

;;;------------------------------------- Machine Utilities ----------------------------------------

(defmethod newloc ((machine machine) n op)
  (with-slots (memory index) machine
    (dotimes (i n)
      (setf (aref memory index) (ldb (octet i) op))
      (incf index))))

(defun format-hex-string (n width)
  (let ((format-string (format nil "~~~D,'0X" width)))
    (format nil format-string n)))

(defmethod print-memory-element ((machine machine) index format)
  (with-slots (memwidth memory pc sp term) machine
    (let ((format-string (ecase format
                           (:latex "~&$\\hex{~A}$ & $\\hex{~A}$ \\\\~%")
                           (:plain "~&~A: ~A~%"))))
      (format t format-string
              (format-hex-string index (ceiling memwidth 4))
              (format-hex-string (aref memory index) 2)))))

(defmethod print-register ((machine machine) register format)
  (with-slots (memwidth pc sp) machine
    (let ((format-string (ecase format
                           (:latex "~&$\\~A$ & $\\hex{~A}$ \\\\~%")
                           (:plain "~&~A: ~A~%")))
          (value (ecase register
                   (pc pc)
                   (sp sp))))
      (format t format-string register (format-hex-string value (ceiling memwidth 4))))))

(defmethod print-machine ((machine machine) &key (format *format*) start end)
  (with-slots (memwidth memory pc sp) machine
    (let ((start (or start 0))
          (end (or end (expt 2 memwidth))))
      (format t "~&PC=~X, SP=~X, memory elements:~%" pc sp)
      (do ((i start (1+ i)))
          ((= i (min end (length memory))))
        (print-memory-element machine i format)))))

(defmethod print-stack ((machine machine))
  (with-slots (memory sp) machine
    (format t "~&Stack:")
    (let ((stack (subseq memory sp)))
      (dotimes (i (length stack))
        (format t " ~2,'0X" (aref stack i))))
    (terpri)))

;;;----------------------------------------- Operations -------------------------------------------

(defclass operation (sb-mop:funcallable-standard-object)
  ((code     :accessor operation-code     :initarg :code)
   (context  :accessor operation-group    :initarg :group)
   (function :accessor operation-function :initarg :function))
  (:default-initargs :code 0 :group nil :function #.(constantly nil))
  (:metaclass sb-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((operation operation) &rest initargs)
  (declare (ignore initargs))
  (with-slots (function) operation
    (sb-mop:set-funcallable-instance-function operation function)))

(defmacro define-opcode (opcode (machine-var (&rest state-vars) &key group) &body body)
  `(setf (gethash ,opcode *operations*)
         (make-instance 'operation
                        :code ,opcode
                        :group ',group
                        :function (lambda (,machine-var)
                                    (with-slots (,@state-vars) ,machine-var
                                      ,@body)))))

;;;-------------------------------------------- Core ----------------------------------------------

(define-opcode +exit+ (m (term) :group core)
  (setq term t))

(define-opcode +nop+ (m () :group core)
  nil)

(define-opcode +jump+ (m (pc) :group core)
  (let ((a (pop* m)))
    (setf pc a)))

(define-opcode +jz_fwd+ (m (pc) :group core)
  (let ((a (fetch m 1)))
    (when (zerop (pop* m))
      (incf pc a))))

(define-opcode +jz_back+ (m (pc) :group core)
  (let ((a (fetch m 1)))
    (when (zerop (pop* m))
      (decf pc (1+ a)))))

(define-opcode +set_sp+ (m (sp) :group core)
  (let ((a (pop* m)))
    (setf sp a)))

(define-opcode +get_pc+ (m (pc) :group core)
  (push* m pc))

(define-opcode +get_sp+ (m (sp) :group core)
  (push* m sp))

(define-opcode +push1+ (m () :group core)
  (let ((a (fetch m 1)))
    (push* m a)))

;;;-------------------------------------- Bit Operations I ----------------------------------------

(define-opcode +load1+ (m () :group bit-1)
  (let* ((a (pop* m))
         (x (get* m 1 a)))
    (push* m x)))

(define-opcode +load2+ (m () :group bit-1)
  (let* ((a (pop* m))
         (x (get* m 2 a)))
    (push* m x)))

(define-opcode +load4+ (m () :group bit-1)
  (let* ((a (pop* m))
         (x (get* m 4 a)))
    (push* m x)))

(define-opcode +load8+ (m () :group bit-1)
  (let* ((a (pop* m))
         (x (get* m 8 a)))
    (push* m x)))

(define-opcode +store1+ (m () :group bit-1)
  (let ((a (pop* m))
        (x (pop* m)))
    (put* m 1 a x)))

(define-opcode +store2+ (m () :group bit-1)
  (let ((a (pop* m))
        (x (pop* m)))
    (put* m 2 a x)))

(define-opcode +store4+ (m () :group bit-1)
  (let ((a (pop* m))
        (x (pop* m)))
    (put* m 4 a x)))

(define-opcode +store8+ (m () :group bit-1)
  (let ((a (pop* m))
        (x (pop* m)))
    (put* m 8 a x)))

;;;---------------------------------------- Arithmetic I ------------------------------------------

(define-opcode +add+ (m () :group math-1)
  (let ((y (pop* m))
        (x (pop* m)))
    (push* m (add x y))))

(define-opcode +mult+ (m () :group math-1)
  (let ((y (pop* m))
        (x (pop* m)))
    (push* m (mult x y))))

(define-opcode +div+ (m () :group math-1)
  (let ((y (pop* m))
        (x (pop* m)))
    (push* m (quotient x y))))

(define-opcode +rem+ (m () :group math-1)
  (let ((y (pop* m))
        (x (pop* m)))
    (push* m (remainder x y))))

;;;--------------------------------------- Arithmetic II ------------------------------------------

(define-opcode +lt+ (m () :group math-2)
  (let ((y (pop* m))
        (x (pop* m)))
    (push* m (lt x y))))

(define-opcode +pow2+ (m () :group math-2)
  (let ((x (pop* m)))
    (push* m (pow2 x))))

;;;--------------------------------------- Boolean Logic ------------------------------------------

(define-opcode +and+ (m () :group logic)
  (let ((y (pop* m))
        (x (pop* m)))
    (push* m (logand x y))))

(define-opcode +or+ (m () :group logic)
  (let ((y (pop* m))
        (x (pop* m)))
    (push* m (logior x y))))

(define-opcode +not+ (m () :group logic)
  (let ((x (pop* m)))
    (push* m (lognot x))))

(define-opcode +xor+ (m () :group logic)
  (let ((y (pop* m))
        (x (pop* m)))
    (push* m (logxor x y))))

;;;------------------------------------- Bit Operations II ----------------------------------------

(define-opcode +push0+ (m () :group bit-2)
  (push* m 0))

(define-opcode +push2+ (m () :group bit-2)
  (let ((a (fetch m 2)))
    (push* m a)))

(define-opcode +push4+ (m () :group bit-2)
  (let ((a (fetch m 4)))
    (push* m a)))

(define-opcode +push8+ (m () :group bit-2)
  (let ((a (fetch m 8)))
    (push* m a)))

;;;----------------------------------------- Utilities --------------------------------------------

(defun entry-count (entry)
  (if (consp entry)
      (destructuring-bind (op . args) entry
        (case op
          (push2 3)
          (push4 5)
          (push8 9)
          (data (length args))
          (t (length entry))))
      entry))

(defun get-address-table (program)
  (let ((counts (mapcar #'entry-count program))
        (table (make-hash-table))
        (address 0))
    (dolist (elem counts)
      (etypecase elem
        (integer (incf address elem))
        (symbol (setf (gethash elem table) address))))
    table))

(defmethod asm ((machine machine) list)
  (let ((address-table (get-address-table list)))
    (flet ((resolve (x)
             (etypecase x
               (integer x)
               (symbol (gethash x address-table)))))
      (with-slots (memory index pc sp term) machine
        (dolist (entry list)
          (etypecase entry
            (symbol (setf index (gethash entry address-table)))
            (cons (destructuring-bind (opcode . args) entry
                    (ecase opcode
                      (exit (newloc machine 1 +exit+))
                      (nop (newloc machine 1 +nop+))
                      (jump (newloc machine 1 +jump+))
                      (jz_fwd (newloc machine 1 +jz_fwd+)
                       (newloc machine 1 (resolve (first args))))
                      (jz_back (newloc machine 1 +jz_back+)
                       (newloc machine 1 (resolve (first args))))
                      (set_sp (newloc machine 1 +set_sp+))
                      (get_pc (newloc machine 1 +get_pc+))
                      (get_sp (newloc machine 1 +get_sp+))
                      (push8 (newloc machine 1 +push8+)
                       (newloc machine 8 (resolve (first args))))
                      (push4 (newloc machine 1 +push4+)
                       (newloc machine 4 (resolve (first args))))
                      (push2 (newloc machine 1 +push2+)
                       (newloc machine 2 (resolve (first args))))
                      (push1 (newloc machine 1 +push1+)
                       (newloc machine 1 (resolve (first args))))
                      (push0 (newloc machine 1 +push0+))
                      (load8 (newloc machine 1 +load8+))
                      (load4 (newloc machine 1 +load4+))
                      (load2 (newloc machine 1 +load2+))
                      (load1 (newloc machine 1 +load1+))
                      (store8 (newloc machine 1 +store8+))
                      (store4 (newloc machine 1 +store4+))
                      (store2 (newloc machine 1 +store2+))
                      (store1 (newloc machine 1 +store1+))
                      (add (newloc machine 1 +add+))
                      (mult (newloc machine 1 +mult+))
                      (div (newloc machine 1 +div+))
                      (rem (newloc machine 1 +rem+))
                      (lt (newloc machine 1 +lt+))
                      (and (newloc machine 1 +and+))
                      (or (newloc machine 1 +or+))
                      (not (newloc machine 1 +not+))
                      (xor (newloc machine 1 +xor+))
                      (pow2 (newloc machine 1 +pow2+))
                      (data (dolist (arg args)
                              (newloc machine 1 arg))))))))))))

(defmethod print-machine-diff ((machine-1 machine) (machine-2 machine) &key (format *format*))
  (with-accessors ((memory-1 machine-memory)
                   (pc-1 machine-pc)
                   (sp-1 machine-sp))
      machine-1
    (with-accessors ((memory-2 machine-memory)
                     (pc-2 machine-pc)
                     (sp-2 machine-sp))
        machine-2
      (assert (= (length memory-1) (length memory-2)))
      (when (/= pc-1 pc-2)
        (print-register machine-1 'pc format))
      (when (/= sp-1 sp-2)
        (print-register machine-1 'sp format))
      (dotimes (i (length memory-1))
        (when (/= (aref memory-1 i) (aref memory-2 i))
          (print-memory-element machine-1 i format))))))

(defmethod machine-copy ((machine machine))
  (with-slots (memwidth groups memory pc sp term) machine
    (let* ((new-machine (make-machine memwidth :groups groups)))
      (dotimes (i (length memory))
        (setf (aref (machine-memory new-machine) i) (aref memory i)))
      (setf (machine-pc new-machine) pc)
      (setf (machine-sp new-machine) sp)
      (setf (machine-term new-machine) term)
      new-machine)))

;;;------------------------------------------- Tests ----------------------------------------------

(defun verify (x)
  (if x
      (format t "~&Success~%")
      (format t "~&FAILURE~%")))

(defmacro test-element (x s)
  (let ((maxint (expt 2 s))
        (i (gensym "I"))
        (y (gensym "Y"))
        (z (gensym "Z"))
        (w (gensym "W")))
    `(dotimes (,i 1000)
       (let* ((,y (random ,maxint))
              (,z (random ,maxint))
              (,w (mod (+ ,y ,z) ,maxint)))
         (setf ,x ,y)
         (unless (= ,x ,y)
           (format t "FAILURE"))
         (increment ,x ,z :size ,s)
         (unless (= ,x ,w)
           (format t "FAILURE"))
         (decrement ,x ,z :size ,s)
         (unless (= ,x ,y)
           (format t "FAILURE"))))))

(defun test-1-elements (memwidth)
  (let ((m (make-machine memwidth)))
    (test-element (machine-pc m) 64)
    (dotimes (i 8)
      (test-element (ldb (octet i) (machine-pc m)) 8))
    (test-element (machine-sp m) 64)
    (dotimes (i 8)
      (test-element (ldb (octet i) (machine-sp m)) 8))
    (dotimes (i (length (machine-memory m)))
      (test-element (aref (machine-memory m) i) 8))))

(defun test-2-put ()
  (let* ((m (make-machine 4 :groups nil)))
    (let ((x #xA7A6A5A4A3A2A1A0))
      (put* m 1 1 x)
      (put* m 2 2 x)
      (put* m 4 4 x)
      (put* m 8 8 x))
    (print-machine m)))

(defun test-2-get ()
  (let* ((m (make-machine 4 :groups nil)))
    (asm m '((data
              #xA0 #xA1 #xA2 #xA3 #xA4 #xA5 #xA6 #xA7
              #xA8 #xA9 #xAA #xAB #xAC #xAD #xAE #xAF)))
    (print-machine m)
    (let ((y (get* m 1 #x01)))
      (verify (= y #x00000000000000A1)))
    (let ((y (get* m 2 #x02)))
      (verify (= y #x000000000000A3A2)))
    (let ((y (get* m 4 #x04)))
      (verify (= y #x00000000A7A6A5A4)))
    (let ((y (get* m 8 #x08)))
      (verify (= y #xAFAEADACABAAA9A8)))))

(defun test-2-push ()
  (let ((m (make-machine 4 :groups nil)))
    (setf (machine-sp m) #x10)
    (let ((x #xAFAEADACABAAA9A8)
          (y #xA7A6A5A4A3A2A1A0))
      (push* m x)
      (verify (= (machine-sp m) #x08))
      (push* m y)
      (verify (= (machine-sp m) #x00)))
    (print-machine m)))

(defun test-2-pop ()
  (let ((m (make-machine 4 :groups nil)))
    (asm m '((data
              #xA0 #xA1 #xA2 #xA3 #xA4 #xA5 #xA6 #xA7
              #xA8 #xA9 #xAA #xAB #xAC #xAD #xAE #xAF)))
    (setf (machine-sp m) 0)
    (print-machine m)
    (let ((x (pop* m)))
      (verify (and (= (machine-sp m) #x08)
                   (= x #xA7A6A5A4A3A2A1A0))))
    (let ((y (pop* m)))
      (verify (and (= (machine-sp m) #x10)
                   (= y #xAFAEADACABAAA9A8))))))

(defun test-2-fetch ()
  (let ((m (make-machine 4 :groups nil)))
    (asm m '((data
              #xA0 #xA1 #xA2 #xA3 #xA4 #xA5 #xA6 #xA7
              #xA8 #xA9 #xAA #xAB #xAC #xAD #xAE #xAF)))
    (print-machine m)
        ;; 1 octet
    (let ((w (fetch m 1)))
      (verify (and (= (machine-pc m) #x01)
                   (= w #x00000000000000A0))))
    ;; 2 octets
    (let ((w (fetch m 2)))
      (verify (and (= (machine-pc m) #x03)
                   (= w #x000000000000A2A1))))
    ;; 4 octets
    (let ((w (fetch m 4)))
      (verify (and (= (machine-pc m) #x07)
                   (= w #x00000000A6A5A4A3))))
    ;; 8 octets
    (let ((w (fetch m 8)))
      (verify (and (= (machine-pc m) #x0F)
                   (= w #xAEADACABAAA9A8A7))))))

(defun test-3-basics ()
  (let ((m (make-machine +memwidth+ :groups '(core))))
    (asm m '((nop)
             (push1 CONTINUE_2)
             (push1 #x00)
             (push1 #x01)
             (push1 CONTINUE_1)
             (push1 #x01)
             (push1 #x00)
             (jz_fwd 1)   ; true
             (exit)
             (jz_back 2)  ; false
             (jump)
             (exit)
             (jump)
             CONTINUE_1
             (jz_fwd 2)   ; false
             (jz_back 4)  ; true
             (exit)
             CONTINUE_2
             (get_sp)
             (get_pc)
             (push1 #xF8)
             (set_sp)
             (exit)))
    (let ((m-orig (machine-copy m)))
      (print-machine m :end (machine-index m))
      (run m)
      (print-machine m :start #xE8)
      #+nil
      (print-machine-diff m m-orig))))

(defun test-4-load-store ()
  (let* ((m (make-machine +memwidth+ :groups '(core bit-1))))
    (asm m `((push1 BLOCK8)
             (load8)
             (push1 #xE0)
             (store8)
             (push1 BLOCK4)
             (load4)
             (push1 #xE8)
             (store4)
             (push1 BLOCK2)
             (load2)
             (push1 #xEC)
             (store2)
             (push1 BLOCK1)
             (load1)
             (push1 #xEE)
             (store1)
             (exit)
             BLOCK8
             (data #xA0 #xA1 #xA2 #xA3 #xA4 #xA5 #xA6 #xA7)
             BLOCK4
             (data #xA8 #xA9 #xAA #xAB)
             BLOCK2
             (data #xAC #xAD)
             BLOCK1
             (data #xAE)))
    (let ((m-orig (machine-copy m)))
      (print-machine m :end (machine-index m))
      (run m)
      (print-machine m)
      #+nil
      (print-machine-diff m m-orig :format :plain))))

(defun test-5-push ()
  (let* ((m (make-machine +memwidth+ :groups '(core bit-2))))
    (asm m `((push0)
             (push2 #x2120)
             (push4 #x13121110)
             (push8 #x0706050403020100)
             (exit)))
    (let ((m-orig (machine-copy m)))
      (print-machine m :end (machine-index m))
      (run m)
      (print-machine m :start #xE0)
      #+nil
      (print-machine-diff m m-orig :format :plain))))

(defun test-6-arithmetic-1 ()
  (let* ((m (make-machine +memwidth+ :groups '(core bit-1 math-1))))
    (asm m `((push1 X)
             (load8)
             (push1 Y)
             (load8)
             (add)
             (push1 X)
             (load8)
             (push1 Y)
             (load8)
             (mult)
             (push1 X)
             (load8)
             (push1 Y)
             (load8)
             (div)
             (push1 X)
             (load8)
             (push1 Y)
             (load8)
             (rem)
             (exit)
             X
             (data #x98 #xE7 #xD9 #x58 #x1B #xC9 #x77 #xFF)
             Y
             (data #x88 #x60 #x09 #x5C #x7D #x2C #x17 #x3F)))
    (let ((m-orig (machine-copy m)))
      (print-machine m :format :plain :end (machine-index m))
      (run m)
      (print-machine m :start #xE0)
      #+nil
      (print-machine-diff m m-orig))))

(defun test-7-arithmetic-2 ()
  (let* ((m (make-machine +memwidth+ :groups '(core bit-1 math-2))))
 (asm m `((push1 Z)
             (load8)
             (push1 Z)
             (load8)
             (lt)
             (push1 Z)
             (load8)
             (push1 W)
             (load8)
             (lt)
             (push1 W)
             (load8)
             (push1 Z)
             (load8)
             (lt)
             (push1 X)
             (load1)
             (pow2)
             (push1 Y)
             (load1)
             (pow2)
             (push1 Z)
             (load1)
             (pow2)
             (exit)
             X
             (data #x40)
             Y
             (data #x22)
             Z
             (data #x00)
             W
             (data #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xA0)))
    (let ((m-orig (machine-copy m)))
      (print-machine m :end (machine-index m))
      (run m)
      (print-machine m :start #xD0)
      #+nil
      (print-machine-diff m m-orig))))

(defun test-8-bitwise-boolean-logic ()
  (let* ((m (make-machine +memwidth+ :groups '(core bit-1 logic))))
    (asm m `((push1 X)
             (load8)
             (push1 Y)
             (load8)
             (and)
             (push1 X)
             (load8)
             (push1 Y)
             (load8)
             (or)
             (push1 X)
             (load8)
             (push1 Y)
             (load8)
             (xor)
             (push1 X)
             (load8)
             (not)
             (exit)
             X
             (data #x98 #xE7 #xD9 #x58 #x1B #xC9 #x77 #xFF)
             Y
             (data #x88 #x60 #x09 #x5C #x7D #x2C #x17 #x3F)))
    (let ((m-orig (machine-copy m)))
      (print-machine m :end (machine-index m))
      (run m)
      (print-machine m :start #xE0)
      #+nil
      (print-machine-diff m m-orig :format :plain))))
