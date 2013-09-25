(defun attack(pos1 pos2)
  (cond
  ((or  (and ( = (car pos1) (car pos2)) (= (cadr pos1) (cadr pos2)))
           (and ( = (abs (- (car pos1) (car pos2))) 1) ( = (abs( - (cadr pos1) (cadr pos2))) 2)) 
           (and ( = (abs (- (car pos1) (car pos2))) 2) ( = (abs( - (cadr pos1) (cadr pos2))) 1)) 
      ) t )
  (t nil)))
      

(defun notallowed(pos noplace)
  (cond
   ((null noplace) nil)
   ((equal pos (car noplace)) t)
   (t (notallowed pos (cdr noplace)))
    )
     )

(defun safe(pos knights noplace)
  (cond
    ((and (null knights) (notallowed pos noplace)) nil)
    ((null knights)
     t)
    ((or (attack pos (car knights)) (notallowed pos noplace))
      nil)
    (t (safe pos (cdr knights) noplace)))
  )

(defun knights_recurse(row col x y noplace start pos sols)
 (cond 
  ((> y col) (knights_recurse row col start 1 noplace (1+ start) pos sols))
  ((and (> x row) (>= (length pos) (maxlengths sols ()))) (cons (reverse pos) sols))
  ;((> x row) (cons (reverse pos) sols))
  ((> x row) sols)

  ((safe (list x y) pos noplace)
   (knights_recurse row col x (1+ y) noplace start pos
    (knights_recurse row col x (1+ y) noplace start 
     (cons (list x y) pos)
     sols)))
  (t (knights_recurse row col x (1+ y) noplace start pos sols))))

(defun maxlengths(lst maxlst)
 (cond
  
  ((null lst) (length maxlst))
  ((> (length (car lst)) (length maxlst)) (maxlengths (cdr lst) (car lst)))
  (t (maxlengths (cdr lst) maxlst))))

(defun place-knights (l)
 (first (knights_recurse (car (car l)) (cadr (car l)) 1 1 (cadr l) 1 '() '())))
