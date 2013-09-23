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

(defun sreck2(row col x y noplace start pos sols)
 (cond 
  ((> y col) (sreck2 row col start 1 noplace (+ start 1) pos sols))
  ((and (> x row) (>= (length pos) (maxlengths sols ()))) (cons (reverse pos) sols))
  ;((> x row) (cons (reverse pos) sols))
  ((> x row) sols)

  ((safe (list x y) pos noplace)
   (sreck2 row col x (+ y 1) noplace start pos
    (sreck2 row col x (+ y 1) noplace start 
     (cons (list x y) pos)
     sols)))
  (t (sreck2 row col x (+ y 1) noplace start pos sols))))

(defun maxlengths(lst maxlst)
 (cond
  
  ((null lst) (length maxlst))
  ((> (length (car lst)) (length maxlst)) (maxlengths (cdr lst) (car lst)))
  (t (maxlengths (cdr lst) maxlst))))

(defun place-knights (l)
 (first (sreck2 (car (car l)) (cadr (car l)) 1 1 (cadr l) 1 '() '())))
