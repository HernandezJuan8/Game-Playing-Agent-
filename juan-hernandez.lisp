;; ASSIGNMENT 4: A MANCALA PLAYER

;; DUE: MONDAY, MAY 2, at MIDNIGHT

;; Juan Hernandez jherna56 G01234626
;; Report at end of the file

;; :P

;;;; Here is a description of the stuff that would go into your
;;;; file.

;;;; The first thing in your function is a package declaration.
;;;; You should name your package something beginning with a colon
;;;; followed by your full name with hyphens for spaces.
;;;; I named my package :sean-luke .  Keep in mind that the name
;;;; is CASE-INSENSITIVE.  The only symbol you should export is the
;;;; symbol COMPUTER-MAKE-MOVE, which should be the name of your top-
;;;; level computer move function.  Name your file the same
;;;; name as your package declaration, minus the colon.  For example,
;;;; my file is named "sean-luke.lisp"

(defpackage :juan-hernandez
  (:use :common-lisp-user :common-lisp)
  (:export computer-make-move))
(in-package :juan-hernandez)


;;;; Once you've done this, you need to write your code.  Here
;;;; is a rough sketch of three functions you will find handy.
;;;; You don't need to implement them like this, except for the
;;;; COMPUTER-MAKE-MOVE function. You can write your code in
;;;; any fashion you like in this file, so long as you do a
;;;; proper alpha-beta heuristic search and your evaluation
;;;; function is stronger than just comparing the differences in
;;;; the mancalas.

(defun alpha-beta (state current-depth max-depth
			 max-player expand terminal evaluate
			 alpha beta)
  "Does alpha-beta search.  Note that there is the addition of
a variable called MAX-PLAYER rather than a function which specifies
if it's max's turn.  It's just more convenient in this system.
The MAX-PLAYER variable is set to either *player-1*
or to *player-2* and indicates if *player-1* or *player-2* should
be considered 'max' (the other player is then considered to be
'min')"
  (if (or (funcall terminal state)
	  (>= current-depth max-depth)) ;; check if we are in max depth or we are terminal
      (funcall evaluate state max-player) ;; if so then call evulate and return what was returned
    (dolist (i (funcall expand state) (if (equalp max-player (state-turn state)) alpha beta)) ;; next moves,  if max turn then return alpha otherwise return beta
      (let ((rec (alpha-beta i (+ current-depth 1) max-depth max-player expand terminal evaluate alpha beta))) ;;some recursive fun to get best state
	(if (equalp max-player (state-turn state)) ;; this if will set the value for either alpha or beta depending on the player
	    (setf alpha (max alpha rec)) ;; alpha set here
	  (setf beta (min beta rec)))) ;; beta set here
      (if (>= alpha beta) ;; if alpha crossed over to beta
	  (return-from alpha-beta (if (equalp max-player (state-turn state)) beta alpha)))))) ;; then return depending on player

(defun evaluate (state max-player)
  "Evaluates the game situation for MAX-PLAYER.
Returns the value of STATE for MAX-PLAYER (who
is either *player-1* or *player-2*).  This should
be a value ranging from *min-wins* to *max-wins*."
  (let ((n (* 2 *num-pits* *initial-stones-per-pit*)) i j m ret) ;; n is number of stones and some cool variables
    (setf i (let ((i (left-pit max-player))) (reduce '+ (subseq (state-board state) i (+ 1 i *num-pits*)))))
    ;; these 2 lines here are similar they grab the sum of the pits the player owns the first one is for max-player
    (setf j (let ((i (left-pit (other-player max-player)))) (reduce '+ (subseq (state-board state) i (+ 1 i *num-pits*))))) ;; this one is for the other player
    (setf ret (+ *min-wins* (/ (* (- (- i j) (- n)) (- *max-wins* *min-wins*)) (- n (- n))))) ;;  this is the main heustric evaluation for the max-player
    ret))

(defun computer-make-move (state max-depth)
  "Given a state, makes a move and returns the new state.
If there is no move to make (end of game) returns nil.
Each time this function calls the top-level
alpha-beta function to search for the quality of a state,
computer-make-move should print out the state (using PRINT,
not PRINT-STATE) that is being searched.
Only search up to max-depth.  The computer should assume
that he is the player who's turn it is right now in STATE"
  (let (b (bs *min-wins*)) ;; bs == score
    (dolist (i (moves state))
      (let ((s (alpha-beta (print i) 0 max-depth (state-turn state) 'moves 'game-overp 'evaluate *min-wins* *max-wins*))) ;; this calls alpha beta and prints out state
	(if (> s bs) (progn (setf b i) (setf bs s))))) ;; saves down b and bs
    b)) ;; return b

;;;; In comments your file, you put your project notes.

;;;; The last thing in your file should be this line (uncommented
;;;; of course).

(in-package :cl-user)

#|
Report for Project
I made the alpha beta function by following algorithm 29 on the lecture notes about 
min-max with alpha beta pruning. It follows closely to that algorithm so it should work as intended.
for evaulate function I made a heurstic evaluation of a given state.  This is done by grabbing how
sum of the pits the player owns and then doing it for the other player. 
Then I return the a hueristiv evaluation that uses both of these using a formula for mancala.
In computer make move function I keep track of the best state and best score , for every state in the list
I do the alpha beta function where if the current score is better than the current best score then ill set the new best and best score with what
the curren one is. I return the best state here. 
The only issue with the code is if there is invalid moves for human player for every move then it will just grid lock with no
fix . Another issue I could not overcome was sometimes the computer didnt do a move that it couldve done meaning it could lose easily
if it is picked up that a move wont occur even though its an obvious mvoe to make. 
Overall This project was good and I learned a good deal  between states and moving between them. I feel Like I did better here than I did with
p3. I lost against the computer but I had a friend play against it and he won so it is definitely beat able if player is good. 
|#


