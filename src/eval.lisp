(in-package :qiqi)

(serapeum:defconst *material-values*
  (list (cons qiku:+pawn+ 100.0)
	(cons qiku:+rook+ 500.0)
	(cons qiku:+knight+ 320.0)
	(cons qiku:+bishop+ 330.0)
	(cons qiku:+queen+ 900.0)
	(cons qiku:+king+ 20000.0)))

(serapeum:defconst +pawn-pst+
  #(  0.0   0.0   0.0   0.0   0.0   0.0   0.0   0.0
     50.0  50.0  50.0  50.0  50.0  50.0  50.0  50.0
     10.0  10.0  20.0  30.0  30.0  20.0  10.0  10.0
      5.0   5.0  10.0  25.0  25.0  10.0   5.0   5.0
      0.0   0.0   0.0  20.0  20.0   0.0   0.0   0.0
      5.0  -5.0 -10.0   0.0   0.0 -10.0  -5.0   5.0
      5.0  10.0  10.0 -20.0 -20.0  10.0  10.0   5.0
      0.0   0.0   0.0   0.0   0.0   0.0   0.0   0.0))

(serapeum:defconst +knight-pst+
  #(-50.0 -40.0 -30.0 -30.0 -30.0 -30.0 -40.0 -50.0
    -40.0 -20.0   0.0   0.0   0.0   0.0 -20.0 -40.0
    -30.0   0.0  10.0  15.0  15.0  10.0   0.0 -30.0
    -30.0   5.0  15.0  20.0  20.0  15.0   5.0 -30.0
    -30.0   0.0  15.0  20.0  20.0  15.0   0.0 -30.0
    -30.0   5.0  10.0  15.0  15.0  10.0   5.0 -30.0
    -40.0 -20.0   0.0   5.0   5.0   0.0 -20.0 -40.0
    -50.0 -40.0 -30.0 -30.0 -30.0 -30.0 -40.0 -50.0))

(serapeum:defconst +bishop-pst+
  #(-20.0 -10.0 -10.0 -10.0 -10.0 -10.0 -10.0 -20.0
    -10.0   0.0   0.0   0.0   0.0   0.0   0.0 -10.0
    -10.0   0.0   5.0  10.0  10.0   5.0   0.0 -10.0
    -10.0   5.0   5.0  10.0  10.0   5.0   5.0 -10.0
    -10.0   0.0  10.0  10.0  10.0  10.0   0.0 -10.0
    -10.0  10.0  10.0  10.0  10.0  10.0  10.0 -10.0
    -10.0   5.0   0.0   0.0   0.0   0.0   5.0 -10.0
    -20.0 -10.0 -10.0 -10.0 -10.0 -10.0 -10.0 -20.0))

(serapeum:defconst +rook-pst+
  #(  0.0   0.0   0.0   0.0   0.0   0.0   0.0   0.0
      5.0  10.0  10.0  10.0  10.0  10.0  10.0   5.0
     -5.0   0.0   0.0   0.0   0.0   0.0   0.0  -5.0
     -5.0   0.0   0.0   0.0   0.0   0.0   0.0  -5.0
     -5.0   0.0   0.0   0.0   0.0   0.0   0.0  -5.0
     -5.0   0.0   0.0   0.0   0.0   0.0   0.0  -5.0
     -5.0   0.0   0.0   0.0   0.0   0.0   0.0  -5.0
      0.0   0.0   0.0   5.0   5.0   0.0   0.0   0.0))

(serapeum:defconst +queen-pst+
  #(-20.0 -10.0 -10.0  -5.0  -5.0 -10.0 -10.0 -20.0
    -10.0   0.0   0.0   0.0   0.0   0.0   0.0 -10.0
    -10.0   0.0   5.0   5.0   5.0   5.0   0.0 -10.0
     -5.0   0.0   5.0   5.0   5.0   5.0   0.0  -5.0
      0.0   0.0   5.0   5.0   5.0   5.0   0.0  -5.0
    -10.0   5.0   5.0   5.0   5.0   5.0   0.0 -10.0
    -10.0   0.0   5.0   0.0   0.0   0.0   0.0 -10.0
    -20.0 -10.0 -10.0  -5.0  -5.0 -10.0 -10.0 -20.0))

(serapeum:defconst +king-pst+
  #(-30.0 -40.0 -40.0 -50.0 -50.0 -40.0 -40.0 -30.0
    -30.0 -40.0 -40.0 -50.0 -50.0 -40.0 -40.0 -30.0
    -30.0 -40.0 -40.0 -50.0 -50.0 -40.0 -40.0 -30.0
    -30.0 -40.0 -40.0 -50.0 -50.0 -40.0 -40.0 -30.0
    -20.0 -30.0 -30.0 -40.0 -40.0 -30.0 -30.0 -20.0
    -10.0 -20.0 -20.0 -20.0 -20.0 -20.0 -20.0 -10.0
     20.0  20.0   0.0   0.0   0.0   0.0  20.0  20.0
     20.0  30.0  10.0   0.0   0.0  10.0  30.0  20.0))

(defun mirror-square (square)
  (+ (* (- 7 (qiku:square-rank square)) 8)
     (qiku:square-file square)))

(defun pst-bonus (piece square)
  (let ((type  (qiku:piece-type  piece))
        (index   (if (= (qiku:piece-color piece) qiku:+white+)
                   square
                   (mirror-square square))))
    (aref (case type
            (#.qiku:+pawn+   +pawn-pst+)
            (#.qiku:+knight+ +knight-pst+)
            (#.qiku:+bishop+ +bishop-pst+)
            (#.qiku:+rook+   +rook-pst+)
            (#.qiku:+queen+  +queen-pst+)
            (#.qiku:+king+   +king-pst+)
            (otherwise       nil))
          index)))

(defun score-piece (piece square)
  (let ((value (+ (cdr (assoc (qiku:piece-type piece) *material-values*))
                  (pst-bonus piece square))))
    (if (= (qiku:piece-color piece) qiku:+white+)
        value
        (- value))))

(defun evaluate (state)
  (iterate
    (for square from 0 to 63)
    (for piece = (qiku:piece-at state square))
    (unless (zerop piece)
      (sum (score-piece piece square)))))
