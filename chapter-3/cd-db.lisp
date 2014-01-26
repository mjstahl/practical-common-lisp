;; simple global variable for our cd database
(defvar *db* nil)

;; database management functions
;; add a new cd to the database
(defun add-record (cd) (push cd *db*))

;; saves the current state of the database to the provided file name
(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

;; load the database from the location on disk provided
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; print the contents of the database in a nicer fashion
(defun dump-db ()
  (dolist (cd *db*)
    ;; t is short for *standard-output* in the format function context
    ;; ~a => aesthetic directive, consume arg and output in human readable form
    ;; ~t => tabulating directive, ~10t move to the 10th column before
    ;;       processing another ~a
    ;; ~{~} =>loop over the list, processing the directives between, consuming
    ;;        as many elements as needed (i.e. consumeing one keyword, and one value)
    ;; ~% => emit one more newline to put a blank line between each CD  
    (format t "~{~a:~10t~a~%~}~%" cd))) 

;; cd creation
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

;; add example data to the database
(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Home" "Dixie Chicks" 7 t))

;; let's make the interaction a little more user friendly
;; user interface functions

;; prompt the user for a piece of information and read it
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;; makes a new CD from data it gets by prompting for each value
(defun prompt-for-cd ()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    ;; parse-integer will signal an error if it can't parse integer
    ;;   out of the string, :junk-allowed allows us to relax the validation
    ;; if we find junk, parse-integer will return nil, we should return 0
    ;;   if we find nil
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
    ;; Y-OR-N-P will reprompt the user if they enter something that doesn't
    ;;  start with y, Y, n, or N
    (y-or-n-p "Ripped? ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
    (if (not (y-or-n-p "Another? ")) (return))))

;; querying functions, for example: 
;;   (select (where :artist "Dixie Chicks"))
;;   (select (where :rating 10 :ripped nil))
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
        (if title    (equal (getf cd :title)  title)  t)
	(if artist   (equal (getf cd :artist) artist) t)
	(if rating   (equal (getf cd :rating) rating) t)
	(if ripped-p (equal (getf cd :ripped) ripped) t))))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))







