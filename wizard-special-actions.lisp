(defun have (object) 
  (member object (cdr (inventory))))

(defparameter *chain-welded* nil)

(defun weld (subject object)
  (if (and (eq *location* 'attic)
           (eq subject 'chain)
           (eq object 'bucket)
           (have 'chain)
           (have 'bucket)
           (not *chain-welded*))
      (progn (setf *chain-welded* t)
             '(the chain is now securely welded to the bucket.))
      '(you cannot weld like that.)))

(pushnew 'weld *allowed-commands*)

(defparameter *bucket-filled* nil)

(defun dunk (subject object)
  (if (and (eq *location* 'garden)
           (eq subject 'bucket)
           (eq object 'well)
           (have 'bucket)
           *chain-welded*)
      (progn (setf *bucket-filled* 't)
             '(the bucket is now full of water))
      '(you cannot dunk like that.)))

(pushnew 'dunk *allowed-commands*)

