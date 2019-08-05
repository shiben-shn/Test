(defun make-1-2-n-sequence (n)
  (let
    ((a (make-sequence 'list n)))
    (do ((i 0 (+ i 1))) ((= i n) a) (setf (nth i a) i))
  )
)

(defun show-board (queens)
  (do* ((i 0 (+ i 1)) (qs queens (rest qs)) (q (first qs) (first qs))) ((null qs) '_)
    (let ((a (make-sequence 'list (length queens) :initial-element '_)))
      (setf (nth (rest (assoc i queens)) a) 'q)
      (print a)
    )
  )
  (print (list 'conflicting 'queens (count-conflicting-queens queens)))
)

(defun assign-queens-random (n)
  (pairlis
    *one-2-n-sequence*
    (map 'list #'(lambda (x) (setf x (random n))) (make-sequence 'list n))
  )
)

(defun count-conflicts-of-one-queen (queens queen-no)
  (let ((queen (assoc queen-no queens))) (- (count-if #'(lambda (x) (or (= (rest queen) (rest x)) (= (abs (- (first queen) (first x))) (abs (- (rest queen) (rest x)))))) queens) 1))
)

(defun count-conflicts (queens)
  (/ (reduce #'+ (map 'list #'(lambda (x) (count-conflicts-of-one-queen queens (first x))) queens)) 2)
)

(defun count-conflicting-queens (queens)
  (count-if #'(lambda (x) (> (count-conflicts-of-one-queen queens (first x)) 0)) queens)
)

(defun step-climb-n-queens (queens)
  (let*
    (
      (n (length queens))
      (c (count-conflicts queens))
      (q (first (nth (random n) queens)))
      (qtry
        (map
          'list
          #'(lambda (x) (substitute (cons (first (assoc q queens)) x) (assoc q queens) queens))
          *one-2-n-sequence*
        )
      )
      (qc
        (map
          'list
          #'(lambda (qx) (count-conflicts-of-one-queen qx q))
          qtry
        )
      )
      (minqc (reduce #'min qc))
      (allminqx (remove 'nil (mapcar #'(lambda (x) (if (= (nth x qc) minqc) x)) *one-2-n-sequence*)))
    )
    (setq selected-config (random (length allminqx)))
    (nth (nth selected-config allminqx) qtry)
  )
)

(defun qmax-conflicted-min-conflicts-n-queens (queens)
  (let*
    (
      (n (length queens))
      (c (count-conflicts queens))
      (c1 (count-conflicting-queens queens))
      (cxlist (mapcar #'(lambda (x) (count-conflicts-of-one-queen queens x)) *one-2-n-sequence*))
      (maxc (reduce #'max cxlist))
      (maxqlist (remove 'nil (mapcar #'(lambda (x) (if (= (nth x cxlist) maxc) x)) *one-2-n-sequence*)))
      (q (nth (random (length maxqlist)) maxqlist))
      (qtry
        (map
          'list
          #'(lambda (x) (substitute (cons (first (assoc q queens)) x) (assoc q queens) queens))
          *one-2-n-sequence*
        )
      )
      (qc
        (map
          'list
          #'(lambda (qx) (count-conflicts-of-one-queen qx q))
          qtry
        )
      )
      (minqc (reduce #'min qc))
      (allminqx (remove 'nil (mapcar #'(lambda (x) (if (= (nth x qc) minqc) x)) *one-2-n-sequence*)))
    )
    (setq selected-config (random (length allminqx)))
    (nth (nth selected-config allminqx) qtry)
  )
)

(defun hill-climb-n-queens (n &key (max-iter-before-restart 100) (max-restart 100))
  (defparameter *one-2-n-sequence* (make-1-2-n-sequence n))
  (setq *random-state* (make-random-state 't))
  (setq oldc n)
  (do
    (
      (iter 1 (+ iter 1))
      (queens (assign-queens-random n) (step-climb-n-queens queens))
      (c n (count-conflicting-queens queens))
    )
    ((= c 0) iter)
    ;(if (< (+ c c) oldc) (print (list 'iteration iter 'conflicts (setq oldc c))))
    ;(if (boundp 'debug-level) (show-board queens))
  )
)

(defun qhill-climb-n-queens (n &key (max-iter-before-restart 100) (max-restart 100))
  (defparameter *one-2-n-sequence* (make-1-2-n-sequence n))
  (setq *random-state* (make-random-state 't))
  (setq oldc n)
  (do
    (
      (iter 1 (+ iter 1))
      (queens (assign-queens-random n) (qmax-conflicted-min-conflicts-n-queens queens))
      (c n (count-conflicting-queens queens))
    )
    ((= c 0) iter)
    ;(if (< (+ c c) oldc) (print (list 'iteration iter 'conflicts (setq oldc c))))
    ;(if (boundp 'debug-level) (show-board queens))
  )
)

(defun simulanneal (queens temperature)
  (let*
    (
      (n (length queens))
      (c1 (count-conflicting-queens queens))
      (cxlist (mapcar #'(lambda (x) (count-conflicts-of-one-queen queens x)) *one-2-n-sequence*))
      (maxc (reduce #'max cxlist))
      (maxqlist (remove 'nil (mapcar #'(lambda (x) (if (= (nth x cxlist) maxc) x)) *one-2-n-sequence*)))
      (changeq (nth (random (length maxqlist)) maxqlist))
      (newqueens (substitute (cons changeq (random n)) (assoc changeq queens) queens))
      (c2 (count-conflicting-queens newqueens))
    )
    (if (> c1 c2)
      newqueens
      (if (<= (random 1000000) (* (exp (/ (- c1 c2) temperature)) 1000000))
        (progn (print (list c1 c2 newqueens)) newqueens)
        queens
      )
    )
  )
)

(defun metropolis (n &key (max-iter-before-restart 100) (max-restart 100) (initial-temperature (* n 100)) (schedule #'(lambda (x) (- x 1))))
  (defparameter *one-2-n-sequence* (make-1-2-n-sequence n))
  (setq *random-state* (make-random-state 't))
  (setq totiter 0)
  (setq restarts 0)
  (do*
    (
      (iter 1 (+ iter 1))
      (temperature initial-temperature (funcall schedule temperature))
      (queens (assign-queens-random n) (simulanneal queens temperature))
      (c n (count-conflicting-queens queens))
    )
    ((= c 0) (list 'success- 'last 'iterations iter 'total 'iterations (+ iter totiter) 'restarts restarts))
    (if (<= temperature 1)
      (progn
        (setq totiter (+ totiter iter))
        (setq iter 1)
        (setq temperature initial-temperature)
        (setq queens (assign-queens-random n))
        (setq restarts (+ restarts 1))
      )
    )
    ;(if (< (+ c c) oldc) (print (list 'iteration iter 'conflicts (setq oldc c))))
    ;(if (boundp 'debug-level) (show-board queens))
  )
)

(defun apply-ga (population)
  (let*
    (
      (popsize (length population))
      (n (length (first population)))
      (changeqlist (mapcar #'(lambda (x) (random n)) *one-2-n-sequence*))
      (oddpop (remove 'nil (mapcar #'(lambda (x) (if (oddp x) (nth x population))) *one-2-n-sequence*)))
      (evenpop (remove 'nil (mapcar #'(lambda (x) (if (evenp x) (nth x population))) *one-2-n-sequence*)))
      (newpop
        (mapcar #'(lambda (x y z) (append (subseq y 0 x) (subseq z x)))
          (subseq changeqlist 0 (length oddpop)) oddpop evenpop
        )
      )
      (newpop1
        (mapcar
          #'(lambda (y)
              (mapcar #'(lambda (x) (if (<= (random 1000000.0) (/ 1000000.0 n)) (cons (first x) (random n)) x)) y)
            )
          (append population newpop)
        )
      )
      (newpop2 (append population newpop newpop1))
      (clist (mapcar #'count-conflicting-queens newpop2))
      (mrglist (sort (pairlis clist newpop2) #'<= :key #'first))
    )
    (mapcar #'rest (subseq mrglist 0 popsize))
  )
)

(defun queenga (n &key (popsize 1))
  (defparameter *one-2-n-sequence* (make-1-2-n-sequence n))
  (setq *random-state* (make-random-state 't))
  (do*
    (
      (rpt 1 (+ rpt 1))
      (population (mapcar #'(lambda (x) (assign-queens-random n)) (make-sequence 'list popsize)) (apply-ga population))
      (clist (mapcar #'count-conflicting-queens population) (mapcar #'count-conflicting-queens population))
    )
    ((numberp (position 0 clist)) (nth (find 0 clist) population))
    (print (list rpt (reduce #'min clist)))
  )
)
