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
  (let ((value (+ (aref +material-values+ (qiku:piece-type piece))
                  (pst-bonus piece square))))
    (if (= (qiku:piece-color piece) qiku:+white+)
        value
        (- value))))

(defun move-priority (move)
  (if (not (zerop (logand (qiku:move-flags move) qiku:+capture-flag+)))
      (- (aref +material-values+ (qiku:piece-type (qiku:move-captured move)))
         (aref +material-values+ (qiku:piece-type (qiku:move-piece    move))))
      -1100.0))

(defun score-isolated-pawns (pawns)
  (iter
    (for file from 0 to 7)
    (for file-mask = (aref qiku:+file-masks+ file))
    (for adjacent-mask = (qiku:adjacent-files-mask file))
    (when (and (not (zerop (logand pawns file-mask)))
	       (zerop (logand pawns adjacent-mask)))
      (sum (* (logcount (logand pawns file-mask)) -30.0)))))

(defun score-doubled-pawns (pawns)
  (iter
    (for file from 0 to 7)
    (for pawns-on-file = (logcount (logand pawns (aref qiku:+file-masks+ file))))
    (sum (* (max 0 (1- pawns-on-file)) -30.0))))

(defun score-passed-pawns (pawns enemy-pawns color)
  (flet ((passed-pawn-mask (square color)
	   (let ((file  (qiku:square-file square))
		 (rank  (qiku:square-rank square)))
	     (iter
	       (with mask = 0)
	       (for r from (if (= color qiku:+white+) (1+ rank) 0)
		    to   (if (= color qiku:+white+) 7 (1- rank)))
	       (for adjacent = (logior (aref qiku:+file-masks+ file)
				       (qiku:adjacent-files-mask file)))
	       (setf mask (logior mask (logand adjacent
					       (iter
						 (for f from 0 to 7)
						 (sum (ash 1 (+ (* r 8) f)))))))
	       (finally (return mask))))))
    (iter
      (for square in (qiku:bb-squares pawns))
      (for rank = (qiku:square-rank square))
      (when (zerop (logand enemy-pawns (passed-pawn-mask square color)))
	(let ((distance (if (= color qiku:+white+)
			    (- 7 rank)
			    rank)))
	  ;; Further is better
	  (sum (aref #(0.0 10.0 30.0 50.0 70.0 100.0 150.0 0.0) distance)))))))

(defun score-pawn-structure (state)
  (let ((white-pawns (qiku:state-white-pawns state))
        (black-pawns (qiku:state-black-pawns state)))
    (- (+ (score-doubled-pawns  white-pawns)
          (score-isolated-pawns white-pawns)
          (score-passed-pawns   white-pawns black-pawns qiku:+white+))
       (+ (score-doubled-pawns  black-pawns)
          (score-isolated-pawns black-pawns)
          (score-passed-pawns   black-pawns white-pawns qiku:+black+)))))

(defun order-moves (moves)
  (sort (copy-list moves) #'> :key #'move-priority))

(defun evaluate (state)
  (+ (score-pawn-structure state)
   (iter
     (for square from 0 to 63)
     (for piece = (qiku:piece-at state square))
     (unless (zerop piece)
       (sum (score-piece piece square))))))

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
  (let ((alpha most-negative-short-float)
	(beta most-positive-short-float))
    (iter
      (for move in (qiku:generate-legal-moves state))
      (qiku:do-move! state move)
      (for score = (- (negamax state (1- depth) (- beta) (- alpha))))
      (qiku:undo-move! state move)
      (finding move maximizing score)
      (setf alpha (max alpha score)))))
