(defun attack(pos1 pos2)
  (if (or  (and ( = (car pos1) (car pos2)) (= (cadr pos1) (cadr pos2)))
           (and ( = (abs (- (car pos1) (car pos2))) 1) ( = (abs( - (cadr pos1) (cadr pos2))) 2)) 
           (and ( = (abs (- (car pos1) (car pos2))) 2) ( = (abs( - (cadr pos1) (cadr pos2))) 1)) 
      ) t )
      )

(defun notallowed(pos noplace)
  (cond
   ((null noplace) nil)
   ((equal pos (car noplace)) t)
   (t (cdr noplace))
    )
     )

(defun safe(pos knights noplace)
  (cond
    ((null knights)
     t)
    ((or (attack pos (car knights)) (notallowed pos noplace))
      nil)
    (t (safe pos (cdr knights) noplace)))
  )

(defun sreck2(row col x y start pos sols)
 (cond 
  ((> y col) (sreck2 row col start 1 (+ start 1) pos sols))
  ;((> x row) (cons (reverse pos) sols))
  ;((and (> x row) (> (length pos) (length (car (last sols))))) (cons (reverse pos) sols))
  ((and (> x row) (> (length pos) (maxlengths sols ()))) (cons (reverse pos) sols))
  ((> x row) sols)

  ((safe (list x y) pos '())
   (sreck2 row col x (+ y 2) start pos
    (sreck2 row col x (+ y 1) start 
     (cons (list x y) pos)
     sols)))
  (t (sreck2 row col x (+ y 1) start pos sols))))

(defun lengths(lst maxlst)
 (cond
  
  ((null lst) maxlst)
  ((> (length (car lst)) (length maxlst)) (lengths (cdr lst) (car lst)))
  (t (lengths (cdr lst) maxlst))))

(defun maxlengths(lst maxlst)
 (cond
  
  ((null lst) (length maxlst))
  ((> (length (car lst)) (length maxlst)) (maxlengths (cdr lst) (car lst)))
  (t (maxlengths (cdr lst) maxlst))))
