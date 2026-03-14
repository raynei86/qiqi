(defpackage :qiqi
  (:use :cl :iterate))
(in-package :qiqi)

(defun uci-send (fmt &rest args)
  (apply #'format t fmt args)
  (terpri)
  (force-output))

(defun parse-move (state move-str)
  (find-if (lambda (move)
             (let ((base (format nil "~a~a"
                                 (qiku:square->algebraic (qiku:move-from move))
                                 (qiku:square->algebraic (qiku:move-to   move)))))
               (if (qiku:move-promotion move)
                   ;; Match the 5th promotion character: q r b n
                   (and (string= base (subseq move-str 0 4))
                        (char= (aref move-str 4)
                               (case (qiku:piece-type (qiku:move-promotion move))
                                 (#.qiku:+queen+  #\q)
                                 (#.qiku:+rook+   #\r)
                                 (#.qiku:+bishop+ #\b)
                                 (#.qiku:+knight+ #\n))))
                   (string= base move-str))))
           (qiku:generate-legal-moves state)))

(defparameter *current-state* (qiku:make-state))

(defun handle-position (tokens)
  (setf *current-state* (qiku:make-state))
  (let ((moves-pos (position "moves" tokens :test #'string=)))
    (when moves-pos
      (apply-moves *current-state* (subseq tokens (1+ moves-pos))))))

(defun apply-moves (state move-tokens)
  (iterate
    (for token in move-tokens)
    (for move = (parse-move state token))
    (when move (qiku:do-move! state move))))

;;; only depth is supported for now
(defun handle-go (tokens)
  (let* ((depth-pos (position "depth" tokens :test #'string=))
         (depth (if depth-pos
		    (parse-integer (nth (1+ depth-pos) tokens))
		    10))
         (move      (best-move *current-state* depth)))
    (if move
        (uci-send "bestmove ~a~a"
                  (qiku:square->algebraic (qiku:move-from move))
                  (qiku:square->algebraic (qiku:move-to   move)))
        (uci-send "bestmove 0000"))))

(defun uci-loop ()
  (iterate
    (for line = (read-line *standard-input* nil nil))
    (while line)
    (for tokens = (str:split #\Space line))
    (for cmd   = (first tokens))
    (for rest  = (rest tokens))
    (cond
      ((string= cmd "isready")  (uci-send "readyok"))
      ((string= cmd "position") (handle-position tokens))
      ((string= cmd "go")       (handle-go rest))
      ((string= cmd "uci")
       (uci-send "id name QIQI")
       (uci-send "id author Lihui Zhang")
       (uci-send "uciok"))
      ((string= cmd "quit")     (return)))))

(defun main ()
  (uci-loop))
