(cl:in-package #:iterate)

(export 'each)

(defmacro-driver (for var each sequence &sequence)
  "TODO"
  (let ((from-var       (gensym "FROM-VAR"))
        (to-var         (gensym "TO-VAR"))
        (above-var      (gensym "ABOVE-VAR"))
        (below-var      (gensym "BELOW-VAR"))
        (sequence-var   (gensym "SEQUENCE-VAR"))
        (iterator-var   (gensym "ITERATOR-VAR"))
        (from-end-var   (gensym "FROM-END-VAR"))
        (step-var       (gensym "STEP-VAR"))
        (end-var        (gensym "END-VAR"))
        (elt-var        (gensym "ELT-VAR"))
        (setelt-var     (gensym "SETELT-VAR"))
        (index-var      (gensym "INDEX-VAR"))
        (limit-var      (gensym "LIMIT-VAR"))
        (mode           (if generate 'generate 'for))
        (down?          (when (or downfrom downto) t)))
    `(progn
       ;; Bound variables
       ,@(when from  `((with ,from-var  = ,from)))
       ,@(when to    `((with ,to-var    = ,to)))
       ,@(when above `((with ,above-var = ,above)))
       ,@(when below `((with ,below-var = ,below)))
       ;; Iteration variables
       (with ,sequence-var = ,sequence)
       (with ,iterator-var)
       (with ,from-end-var)
       (with ,step-var)
       (with ,end-var)
       (with ,elt-var)
       (with ,setelt-var)
       (with ,index-var)
       (with ,limit-var)
       ,@(when with-index
           `((with (the (and fixnum (integer 0)) ,with-index)))) ; TODO(jmoringe): or integer
       ;; Iterator creation
       (when (first-iteration-p)
         (setf (values ,iterator-var ,limit-var ,from-end-var
                       ,step-var ,end-var ,elt-var ,setelt-var ,index-var)
               (sequence:make-sequence-iterator
                ,sequence-var
                ,@(when from     (if down?
                                     `(:end   (when ,from-var (1+ ,from-var)))
                                     `(:start ,from-var)))
                ,@(when above    (if down?
                                     `(:end   (when ,above-var (+ ,above-var 2))) ; TODO(jmoringe, 2012-08-14): avoid if ABOVE is known to be (not null)
                                     `(:start (when ,above-var (1+ ,above-var)))))
                ,@(when downto   `(:start ,downto))
                ,@(when to       (if down?
                                     `(:start ,to-var)
                                     `(:end   (when ,to-var (1+ ,to-var)))))
                ,@(when below    (if down?
                                     `(:start (when ,below-var (1- ,below-var)))
                                     `(:end   ,below-var)))
                ,@(when downfrom `(:end (when ,downfrom (1+ ,downfrom))))
                :from-end ,down?)))
       ;; Step clause
       (,mode ,var next
              (progn
                (unless (first-iteration-p)
                  ,(make-stepper by sequence-var iterator-var
                                 step-var end-var limit-var from-end-var))
                ,(make-terminate sequence-var iterator-var
                                 end-var limit-var from-end-var)
                ,@(when with-index
                    `((setf ,with-index (the (integer 0 *) ; TODO(jmoringe): fixnum, no?
                                             (funcall ,index-var ,sequence-var ,iterator-var)))))
                (funcall ,elt-var ,sequence-var ,iterator-var))))))

;;; Utility functions

(defun make-terminate (sequence iterator end limit from-end
                       &optional (body '((terminate))))
  ;; Return forms which execute BODY when the iteration described by
  ;; SEQUENCE, ITERATOR, END, LIMIT and FROM-END should terminate.
  `(when (funcall (the function ,end)
                  ,sequence ,iterator ,limit ,from-end)
     ,@body))

(defun make-stepper (by sequence iterator step end limit from-end)
  (if (or (not by) (and (realp by) (= (abs by) 1))) ; TODO(jmoringe, 2012-08-14): be smarter about constants
      (make-stepper/1 sequence iterator step from-end)
      (make-stepper/>1 by sequence iterator step end limit from-end)))

(defun make-stepper/1 (sequence iterator step from-end)
  `(setf ,iterator (funcall (the function ,step)
                            ,sequence ,iterator ,from-end)))

(defun make-stepper/>1 (by sequence iterator step end limit from-end)
  (let ((i-var (gensym "I")))
   `(catch 'exhausted
      (dotimes (,i-var (abs ,by))
        (unless (zerop ,i-var)
          (when ,(make-terminate sequence iterator end limit from-end
                                 '((throw 'exhausted nil)))))
        ,(make-stepper/1 sequence iterator step from-end)))))
