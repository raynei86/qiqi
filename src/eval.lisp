(in-package :qiqi)

(defun mirror-square (square)
  (+ (* (- 7 (qiku:square-rank square)) 8)
     (qiku:square-file square)))

(defun pst-bonus (piece square)
  (let ((type  (qiku:piece-type  piece))
        (index   (if (= (qiku:piece-color piece) qiku:+white+)
                   (mirror-square square)
		   square)))
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

(defun move-priority (move)
  (if (qiku:move-captured move)
      (- (getf *material-values* (qiku:piece-type (qiku:move-captured move)) 0.0)
         (getf *material-values* (qiku:piece-type (qiku:move-piece    move)) 0.0))
      -1000.0))

(defun order-moves (moves)
  (sort (copy-list moves) #'> :key #'move-priority))

(defun evaluate (state)
  (iter
    (for square from 0 to 63)
    (for piece = (qiku:piece-at state square))
    (unless (zerop piece)
      (sum (score-piece piece square)))))

(defun quiescence (state alpha beta)
  (let ((stand-pat (evaluate state)))
    (when (>= stand-pat beta) (return-from quiescence beta))
    (setf alpha (max alpha stand-pat))
    (iter
      (for move in (order-moves
                     (remove-if-not #'qiku:move-captured
                                    (qiku:generate-legal-moves state))))
      (qiku:do-move! state move)
      (for score = (- (quiescence state (- beta) (- alpha))))
      (qiku:undo-move! state move)
      (when (>= score beta) (return beta))
      (setf alpha (max alpha score))
      (finally (return alpha)))))

(defun negamax (state depth alpha beta)
  (let ((moves (qiku:generate-legal-moves state)))
    (cond
      ((qiku:checkmate-p state moves) most-negative-short-float)
      ((qiku:stalemate-p state moves) 0.0)
      ((zerop depth) (quiescence state alpha beta))

      (t
       (iter
         (for move in (order-moves moves))
         (qiku:do-move! state move)
         (for score = (- (negamax state (1- depth) (- beta) (- alpha))))
         (qiku:undo-move! state move)
         (maximizing score into best)
         (setf alpha (max alpha best))
         (when (>= alpha beta) (finish))
         (finally (return best)))))))

(defun best-move (state depth)
  (let ((alpha most-positive-short-float)
	(beta most-negative-short-float))
    (iter
      (for move in (qiku:generate-legal-moves state))
      (qiku:do-move! state move)
      (for score = (- (negamax state (1- depth) (- beta) (- alpha))))
      (qiku:undo-move! state move)
      (finding move maximizing score)
      (setf alpha (max alpha score)))))
