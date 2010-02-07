:; The pattern function hash
(defvar pattern-keys      '())
(defvar pattern-functions (make-hash-table))

(defmacro defpattern (start-char argument-list &rest body)
  ; defpattern should be indented like defun
  (declare (indent defun))

  ; Validate argument types
  (if (not (char-valid-p start-char)) 
      (error "Movement command definition expected single-char pattern name"))
  (if (not (listp argument-list))
      (error "Movement command definition expected argument list"))
  
  ; The argument list should have either two arguments (start/end points)
  ; Or it should have one argument, the total movement command argument
  (let ((arglen (length argument-list)))
    (if (not (or (eq arglen 2) (eq arglen 1)))
	(error "Movement command argument list invalid.")))

  ; Add the pattern function to the hash
  `(progn
     (setq pattern-keys (cons ,start-char pattern-keys))
     (puthash 
      ,start-char 
      '(,(length argument-list) (lambda ,argument-list ,@body))
      pattern-functions)))

; Allow repeats of commands
(defvar last-movement-command "")
(defun repeat-last-movement-command ()
  (pattern-command last-movement-command))

; Delete pattern 'd'
(defpattern ?d (start-point end-point) 
  (kill-region start-point end-point))

; Upcase pattern 'u'
(defpattern ?u (start-point end-point)
  (upcase-region start-point end-point))

; Downcase pattern 'l'
(defpattern ?l (start-point end-point)
  (downcase-region start-point end-point))

; Search/replace pattern 's'
(defpattern ?s (pattern)
  (if (equal 0 (length pattern)) (error "Empty search pattern"))

  (let ((pattern-end-index 0))
    ; Find the index where there is a / not preceeded by a \
    (while (not (and (equal (aref (vector pattern) pattern-end-index) ?/) (not (equal (aref (vector pattern) (- pattern-end-index 1)) ?\\))))
      (incf pattern-end-index))
    
    ))

; searching
(defvar previous-movement-search '())
(defun movement-search-forward (pattern)
  (setq previous-movement-search `(forward ,pattern))
  (search-forward-regexp pattern)
  (search-backward-regexp pattern))

(defun movement-search-backward (pattern)
  (setq previous-movement-search `(backward ,pattern))
  (search-backward-regexp pattern)
  (search-forward-regexp pattern))

(defun repeat-last-search-movement ()
  (case (car previous-movement-search)
    (('backward) (movement-search-backward (cadr previous-movement-search)))
    (('forward)  (movement-search-forward  (cadr previous-movement-search)))))

; Char is a digit?
(defun digitp (char)
  (case char
	((?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9) t)
	(otherwise nil)))

; Movement handler
(defun movement (command)
  (if (equal command "") (return-from movement))
  (let ((next-substring (substring command 1)))
  (case (aref (vconcat command) 0)
	; Search movements
	((?/) (movement-search-forward  next-substring))
	((??) (movement-search-backward next-substring))
	
	; Line and character movements
	((?w) (previous-line) (movement next-substring))
	((?a) (backward-char) (movement next-substring))
	((?s) (next-line)     (movement next-substring))
	((?d) (forward-char)  (movement next-substring))
	
	; Repeat last command
	((?.) (repeat-last-movement-command))
	
	; Repeated movements
	((?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
	 (let ((repeats (car (read-from-string command))))
	   (dotimes (repeat-num repeats)
	     (movement next-substring)))))))

(defun pattern-command (command)
  (interactive "sCommand: ")
  ; Input validation
  (if (eq (length command) 0) (error "Empty command"))
  (setq last-movement-command command)
  (setq found nil)
  (dolist (char pattern-keys)
    (if (eq char (aref (vconcat command) 0))
	(let* ((hash-value (gethash char pattern-functions)) 
	       (num-args (car hash-value))
	       (func (cadr hash-value)))
	  (setq found t)
	  (if (eq num-args 2)
	      (funcall func (point) (movement (substring command 1)))
	    (funcall func (substring command 1))))))


	  ; If not found, just use the movement
          (if (not found) (movement command)))