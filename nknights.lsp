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

(defun check_col(lst row col noplace)
  (cond
    ((or (null row) (null col)) lst)
    ((safe (list row (car col)) lst noplace) (check_col (cons (list row (car col)) lst) row (cdr col) noplace))
    (t (check_col lst row (cdr col) noplace))
    )
  )

(defun solve_2(lst rows cols noplace)
   (cond
     ((null rows) lst)
     ( t (list lst (solve_2 (check_col lst (car rows) cols noplace) (cdr rows) cols noplace)))
     )
   )

(defun solve_7(lst rows cols noplace n)
  (cond
    ((= n 0) lst)
    (t (solve_7 
       (loop for i from 1 to rows do
         (loop for j from 0 to cols do
          (if (safe (list i (+ j 1)) lst noplace)
               (list lst (list i (+ j 1))))
                )
                 ) rows cols noplace (- n 1)))
                  )
  )

(defun srec(sz x y pos sols)
 (cond 
  ; If we've advanced past the last column, we have a solution.
  ; (By the way, the reverse is because pos is built up backward.)
  ((> x sz) (cons (reverse pos) sols))
  ; If we've advanced past the last row, we have a failure.
  ((> y sz) sols)
  ; If the queen is safe, the fun begins.
  ((safe (list x y) pos '())
   ; This is the backtracking call. This is executed once
   ; the inner call is complete.
   (srec sz x (+ y 1) pos
    ; Run the next column first; if any solutions
    ; result, they need to be passed to the backtracked
    ; call.
    (srec sz (+ x 1) 1
     ; Add this queen when considering the next
     ; column's placement.
     (cons (list x y) pos)
     sols)))
  ; If this queen isn't safe, move on to the next row.
  (t (srec sz x (+ y 1) pos sols))))

(defun sreck(sz x y start pos sols)
 (cond 
  ; If we've advanced past the last column, we have a solution.
  ; (By the way, the reverse is because pos is built up backward.)
  ((> x sz) (cons (reverse pos) sols))
  ; If we've advanced past the last row, we have a failure.
  ((> y sz) (sreck sz (+ start 1) 1 (+ start 1) pos sols))

  ((> start sz) sols)
  ; If the queen is safe, the fun begins.
  ((safe (list x y) pos '())
   ; This is the backtracking call. This is executed once
   ; the inner call is complete.
   (sreck sz x (+ y 1) start pos
    ; Run the next column first; if any solutions
    ; result, they need to be passed to the backtracked
    ; call.
    (sreck sz (+ x 1) 1 start 
     ; Add this queen when considering the next
     ; column's placement.
     (cons (list x y) pos)
     sols)))
  ; If this queen isn't safe, move on to the next row.
  (t (sreck sz x (+ y 1) start pos sols))))

(defun sreck1(sz x y start pos sols)
 (cond 
  ; If we've advanced past the last column, we have a solution.
  ; (By the way, the reverse is because pos is built up backward.)
  ; If we've advanced past the last row, we have a failure.
  ;((> y sz) (- y (+ sz 1)))
  ((> y sz) (sreck1 sz start 1 (+ start 1) pos sols))
  ((> x sz) (cons (reverse pos) sols))

  ; If the queen is safe, the fun begins.
  ((safe (list x y) pos '())
   ; This is the backtracking call. This is executed once
   ; the inner call is complete.
   (sreck1 sz x (+ y 2) start pos
    ; Run the next column first; if any solutions
    ; result, they need to be passed to the backtracked
    ; call.
    (sreck1 sz x (+ y 1) start 
     ; Add this queen when considering the next
     ; column's placement.
     (cons (list x y) pos)
     sols)))
  ; If this queen isn't safe, move on to the next row.
  (t (sreck1 sz x (+ y 1) start pos sols))))

(defun sreck2(row col x y start pos sols)
 (cond 
  ((> y col) (sreck2 row col start 1 (+ start 1) pos sols))
  ((> x row) (cons (reverse pos) sols))

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
