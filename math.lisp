(load "lazy")

; Square root by Newton's method
; > (mysqrt 1 0.001 4.0)
; > (mysqrt 1 0.001 200.0)

(defun repeat (f a)
  (lazy-cons a (repeat f (funcall f a))))

(defun next (n)
  (lambda (x) (/ (+ x (/ n x)) 2)))

(defun within (eps lst)
  (if (<= (abs (- (lazy-car lst) (lazy-cadr lst))) eps)
      (lazy-cadr lst)
      (within eps (lazy-cdr lst))))

(defun mysqrt (a eps n)
  (within eps
          (repeat (next n) a)))

