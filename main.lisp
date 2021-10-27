(cl:in-package #:transclime)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Normalization of sequences of characters

(defparameter *normalizations*
  '(((#\a #.(code-char 768)) . #\à)
    ((#\a #.(code-char 769)) . #\á)
    ((#\a #.(code-char 771)) . #\ã)
    ((#\a #.(code-char 777)) . #\ả)
    ((#\a #.(code-char 803)) . #\ạ)

    ((#\A #.(code-char 768)) . #\À)
    ((#\A #.(code-char 769)) . #\Á)
    ((#\A #.(code-char 771)) . #\Ã)
    ((#\A #.(code-char 777)) . #\Ả)
    ((#\A #.(code-char 803)) . #\Ạ)

    ((#\â #.(code-char 768)) . #\ầ)
    ((#\â #.(code-char 769)) . #\ấ)
    ((#\â #.(code-char 771)) . #\ẫ)
    ((#\â #.(code-char 777)) . #\ẩ)
    ((#\â #.(code-char 803)) . #\ậ)

    ((#\Â #.(code-char 768)) . #\Ầ)
    ((#\Â #.(code-char 769)) . #\Ấ)
    ((#\Â #.(code-char 771)) . #\Ẫ)
    ((#\Â #.(code-char 777)) . #\Ẩ)
    ((#\Â #.(code-char 803)) . #\Ậ)

    ((#\ă #.(code-char 768)) . #\ằ)
    ((#\ă #.(code-char 769)) . #\ắ)
    ((#\ă #.(code-char 771)) . #\ẵ)
    ((#\ă #.(code-char 777)) . #\ẳ)
    ((#\ă #.(code-char 803)) . #\ặ)

    ((#\Ă #.(code-char 768)) . #\Ằ)
    ((#\Ă #.(code-char 769)) . #\Ắ)
    ((#\Ă #.(code-char 771)) . #\Ẵ)
    ((#\Ă #.(code-char 777)) . #\Ẳ)
    ((#\Ă #.(code-char 803)) . #\Ặ)

    ((#\o #.(code-char 768)) . #\ò)
    ((#\o #.(code-char 769)) . #\ó)
    ((#\o #.(code-char 771)) . #\õ)
    ((#\o #.(code-char 777)) . #\ỏ)
    ((#\o #.(code-char 803)) . #\ọ)

    ((#\O #.(code-char 768)) . #\Ò)
    ((#\O #.(code-char 769)) . #\Ó)
    ((#\O #.(code-char 771)) . #\Õ)
    ((#\O #.(code-char 777)) . #\Ỏ)
    ((#\O #.(code-char 803)) . #\Ọ)

    ((#\ô #.(code-char 768)) . #\ồ)
    ((#\ô #.(code-char 769)) . #\ố)
    ((#\ô #.(code-char 771)) . #\ỗ)
    ((#\ô #.(code-char 777)) . #\ổ)
    ((#\ô #.(code-char 803)) . #\ộ)

    ((#\Ô #.(code-char 768)) . #\Ồ)
    ((#\Ô #.(code-char 769)) . #\Ố)
    ((#\Ô #.(code-char 771)) . #\Ỗ)
    ((#\Ô #.(code-char 777)) . #\Ổ)
    ((#\Ô #.(code-char 803)) . #\Ộ)

    ((#\ơ #.(code-char 768)) . #\ờ)
    ((#\ơ #.(code-char 769)) . #\ớ)
    ((#\ơ #.(code-char 771)) . #\ỡ)
    ((#\ơ #.(code-char 777)) . #\ở)
    ((#\ơ #.(code-char 803)) . #\ợ)
    
    ((#\Ơ #.(code-char 768)) . #\Ờ)
    ((#\Ơ #.(code-char 769)) . #\Ớ)
    ((#\Ơ #.(code-char 771)) . #\Ỡ)
    ((#\Ơ #.(code-char 777)) . #\Ở)
    ((#\Ơ #.(code-char 803)) . #\Ợ)
    
    ((#\u #.(code-char 768)) . #\ù)
    ((#\u #.(code-char 769)) . #\ú)
    ((#\u #.(code-char 771)) . #\ũ)
    ((#\u #.(code-char 777)) . #\ủ)
    ((#\u #.(code-char 803)) . #\ụ)
    
    ((#\U #.(code-char 768)) . #\Ù)
    ((#\U #.(code-char 769)) . #\Ú)
    ((#\U #.(code-char 771)) . #\Ũ)
    ((#\U #.(code-char 777)) . #\Ủ)
    ((#\U #.(code-char 803)) . #\Ụ)
    
    ((#\ư #.(code-char 768)) . #\ừ)
    ((#\ư #.(code-char 769)) . #\ứ)
    ((#\ư #.(code-char 771)) . #\ữ)
    ((#\ư #.(code-char 777)) . #\ử)
    ((#\ư #.(code-char 803)) . #\ự)
    
    ((#\Ư #.(code-char 768)) . #\Ừ)
    ((#\Ư #.(code-char 769)) . #\Ứ)
    ((#\Ư #.(code-char 771)) . #\Ữ)
    ((#\Ư #.(code-char 777)) . #\Ử)
    ((#\Ư #.(code-char 803)) . #\Ự)
    
    ((#\e #.(code-char 768)) . #\è)
    ((#\e #.(code-char 769)) . #\é)
    ((#\e #.(code-char 771)) . #\ẽ)
    ((#\e #.(code-char 777)) . #\ẻ)
    ((#\e #.(code-char 803)) . #\ẹ)
    
    ((#\E #.(code-char 768)) . #\È)
    ((#\E #.(code-char 769)) . #\É)
    ((#\E #.(code-char 771)) . #\Ẽ)
    ((#\E #.(code-char 777)) . #\Ẻ)
    ((#\E #.(code-char 803)) . #\Ẹ)
    
    ((#\ê #.(code-char 768)) . #\ế)
    ((#\ê #.(code-char 769)) . #\ế)
    ((#\ê #.(code-char 771)) . #\ễ)
    ((#\ê #.(code-char 777)) . #\ể)
    ((#\ê #.(code-char 803)) . #\ệ)
    
    ((#\Ê #.(code-char 768)) . #\Ế)
    ((#\Ê #.(code-char 769)) . #\Ế)
    ((#\Ê #.(code-char 771)) . #\Ễ)
    ((#\Ê #.(code-char 777)) . #\Ể)
    ((#\Ê #.(code-char 803)) . #\Ệ)
    
    ((#\i #.(code-char 768)) . #\ì)
    ((#\i #.(code-char 769)) . #\í)
    ((#\i #.(code-char 771)) . #\ĩ)
    ((#\i #.(code-char 777)) . #\ỉ)
    ((#\i #.(code-char 803)) . #\ị)
    
    ((#\I #.(code-char 768)) . #\Ì)
    ((#\I #.(code-char 769)) . #\Í)
    ((#\I #.(code-char 771)) . #\Ĩ)
    ((#\I #.(code-char 777)) . #\Ỉ)
    ((#\I #.(code-char 803)) . #\Ị)
    
    ((#\y #.(code-char 768)) . #\ỳ)
    ((#\y #.(code-char 769)) . #\ý)
    ((#\y #.(code-char 771)) . #\ỹ)
    ((#\y #.(code-char 777)) . #\ỷ)
    ((#\y #.(code-char 803)) . #\ỵ)

    ((#\Y #.(code-char 768)) . #\Ỳ)
    ((#\Y #.(code-char 769)) . #\Ý)
    ((#\Y #.(code-char 771)) . #\Ỹ)
    ((#\Y #.(code-char 777)) . #\Ỷ)
    ((#\Y #.(code-char 803)) . #\Ỵ)))
    
(defclass wrapped-stream ()
  ((%stream :initarg :stream :reader wrapped-stream)
   (%buffer :initform '() :accessor buffer)))

(defun wrap-stream (stream)
  (make-instance 'wrapped-stream :stream stream))

(defun read-char-wrapped (stream)
  (case (length (buffer stream))
    (0 (let ((char (read-char (wrapped-stream stream) nil nil)))
	 (cond ((null char)
		nil)
	       ((find char *normalizations* :key #'caar)
		(let ((char2 (read-char (wrapped-stream stream) nil nil)))
		  (cond ((null char2)
			 char)
			((member (char-code char2) '(768 769 771 777 803))
			 (cdr (assoc (list char char2)
				     *normalizations*
				     :test #'equal)))
			(t
			 (progn (setf (buffer stream) (list char2))
				char)))))
	       (t char))))
    (1 (cond ((find (car (buffer stream)) *normalizations* :key #'caar)
	      (let ((char2 (read-char (wrapped-stream stream) nil nil)))
		(cond ((null char2)
		       (pop (buffer stream)))
		      ((member (char-code char2) '(768 769 771 777 803))
		       (cdr (assoc (list (pop (buffer stream)) char2)
				   *normalizations*
				   :test #'equal)))
		      (t
		       (prog1 (pop (buffer stream))
			 (setf (buffer stream) (list char2)))))))
	     (t
	      (pop (buffer stream)))))
    (2 (pop (buffer stream)))))

(defun unread-char-wrapped (char stream)
  (push char (buffer stream)))

(defun read-line-wrapped (stream)
  (with-output-to-string (string-stream)
    (loop for char = (read-char-wrapped stream)
	  until (or (null char) (eql char #\Newline))
	  do (write-char char string-stream))
    string-stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The rest of the application

(defparameter *dico* '())

(defclass word-entry ()
  ((dico-entries :initarg :dico-entries :reader dico-entries)))

(defgeneric word-entry-p (object)
  (:method (object) nil)
  (:method ((object word-entry)) t))

(define-application-frame transclime ()
  ((%soft-match :initform nil :accessor soft-match)
   (%fix-entry :initform nil :accessor fix-entry))
  (:panes
   (text :application
	 :scroll-bars nil
	 :width 800
	 :display-time nil)
   (dico :application
	 :scroll-bars nil
	 :width 700 :display-time nil)
   (inter :interactor :width 1500 :height 100))
  (:layouts
   (:default (vertically ()
	       (horizontally (:height 1000)
		 (scrolling (:scroll-bars t) text)
		 (scrolling (:scroll-bars t) dico))
	       inter))))
			   
(defun transclime ()
  (run-frame-top-level (make-application-frame 'transclime)))

;;; Just so that it can be traced. 
(defun my-alpha-char-p (char)
  (alpha-char-p char))

(define-transclime-command (com-quit :name t) ()
  (frame-exit *application-frame*))

;;; Return the text of the stream as a list of alternating
;;; words (strings containing only alphabetic characters), 
;;; and non-words (string containing only non-alphabetic
;;; characters). 
(defun read-text (stream)
  (flet ((read-uniform-sequence (string test)
	   (loop for char = (read-char-wrapped stream)
		 while (and (not (null char))
			    (funcall test char))
		 do (write-char char string)
		 finally (unless (null char)
			   (unread-char-wrapped char stream)))))
    (loop for char = (read-char-wrapped stream)
	  until (null char)
	  do (unread-char-wrapped char stream)
	  collect (with-output-to-string (string)
		    (read-uniform-sequence
		     string
		     (if (alpha-char-p char)
			 #'alpha-char-p
			 (complement #'alpha-char-p)))))))

(define-transclime-command (com-read-dico :name t)
    ((filename 'pathname
               :default (namestring (first (directory ".")))
               :default-type 'pathname
               :insert-default t))
  (with-open-file (stream filename :direction :input)
    (setf *dico* (merge-dicos *dico* (read-dico (wrap-stream stream))))))
      
(define-transclime-command (com-clear-dico :name t) ()
  (setf *dico* '()))

(defparameter *translate*
  '((#\á . #\a) (#\à . #\a) (#\ã . #\a) (#\ạ . #\a) (#\ả . #\a)
    (#\â . #\a) (#\ấ . #\a) (#\ầ . #\a) (#\ẫ . #\a) (#\ậ . #\a) (#\ẩ . #\a)
    (#\ă . #\a) (#\ắ . #\a) (#\ằ . #\a) (#\ẵ . #\a) (#\ặ . #\a) (#\ẳ . #\a)
    (#\é . #\e) (#\è . #\e) (#\ẽ . #\e) (#\ẹ . #\e) (#\ẻ . #\e)
    (#\ê . #\e) (#\ế . #\e) (#\ề . #\e) (#\ễ . #\e) (#\ệ . #\e) (#\ể . #\e)
    (#\í . #\i) (#\ì . #\i) (#\ĩ . #\i) (#\ị . #\i) (#\ỉ . #\i)
    (#\ó . #\o) (#\ò . #\o) (#\õ . #\o) (#\ọ . #\o) (#\ỏ . #\o) 
    (#\ô . #\o) (#\ố . #\o) (#\ồ . #\o) (#\ỗ . #\o) (#\ộ . #\o) (#\ổ . #\o)
    (#\ơ . #\o) (#\ớ . #\o) (#\ờ . #\o) (#\ỡ . #\o) (#\ợ . #\o) (#\ở . #\o)
    (#\ú . #\u) (#\ù . #\u) (#\ũ . #\u) (#\ụ . #\u) (#\ủ . #\u)
    (#\ư . #\u) (#\ứ . #\u) (#\ừ . #\u) (#\ữ . #\u) (#\ự . #\u) (#\ử . #\u)
    (#\ý . #\y) (#\ỳ . #\y) (#\ỹ . #\y) (#\ỵ . #\y) (#\ỷ . #\y)
    (#\đ . #\d)))

(defun de-accentify-char (char)
  (or (cdr (assoc char *translate* :test #'char-equal)) char))

(defun de-accentify-string (string)
  (map 'string
       #'de-accentify-char
       string))

(defun string-compare (s1 s2)
  (if (soft-match *application-frame*)
      (string-equal (de-accentify-string s1) (de-accentify-string s2))
      (string-equal s1 s2)))

(define-transclime-command (com-read-text :name t)
    ((filename 'pathname
               :default (namestring (first (directory ".")))
               :default-type 'pathname
               :insert-default t))
  (with-open-file (stream filename :direction :input)
    (let* ((out (find-pane-named *application-frame* 'text))
	   (medium (sheet-medium out))
	   (text (read-text (wrap-stream stream)))
	   (text-style (make-text-style :sans-serif :roman nil))
	   (words (remove-if (lambda (string)
			       (or (zerop (length string))
				   (not (alpha-char-p (aref string 0)))))
			     text))
	   (hash-dico (let ((dico (make-hash-table :test #'equalp)))
			(loop for entry in *dico*
			      do (let ((word (funcall (if (soft-match *application-frame*)
							  #'de-accentify-string
							  #'identity)
						      (car (original-words entry)))))
				   (push entry (gethash word dico))))
			dico)))
      (window-clear out)
      (setf (stream-drawing-p out) nil)
      (with-text-style (medium text-style)
	(with-text-size (medium :huge)
	  (loop for string in text
		do (if (or (zerop (length string))
			   (not (alpha-char-p (aref string 0))))
		       (write-string string out)
		       (let ((dico-entries
			      (remove-if-not
			       (lambda (dico-entry)
				 (and (every #'string-compare
					     words
					     (original-words dico-entry))
				      (not (null (nthcdr (1- (length (original-words dico-entry))) words)))))
			       (gethash (funcall (if (soft-match *application-frame*)
							  #'de-accentify-string
							  #'identity)
						 (first words))
					hash-dico))))
;;			       *dico*)))
			 (pop words)
			 (if (null dico-entries)
			     (with-drawing-options (out :ink +red+)
			       (write-string string out))
			     (with-output-as-presentation
				 (out (make-instance 'word-entry
						     :dico-entries dico-entries)
				      'word-entry)
			       (write-string string out))))))
	  (finish-output out)))
      (setf (stream-drawing-p out) t)
      (replay (stream-output-history out) out))))

(defun show-entry (word-entry)
  (unless (fix-entry *application-frame*)
    (let* ((stream (find-pane-named *application-frame* 'dico ))
	   (medium (sheet-medium stream))
	   (text-style (make-text-style :sans-serif :roman nil)))
      (with-text-style (medium text-style)
	(flet ((show-meaning (meaning)
		 (with-drawing-options (medium :ink +red+)
		   (format stream "   ~a~%" (translation meaning)))
		 (unless (null (examples meaning))
		   (format stream "     Examples:~%"))
		 (loop for example in (examples meaning)
		       do (format stream "      ~a~%" (original example))
		       do (unless (null (translation example))
			    (with-drawing-options (medium :ink +orange+)
			      (format stream "      ~a~%" (translation example)))))))
	  (loop for entry in (dico-entries word-entry)
		do (with-text-face (medium :bold)
		     (loop for word in (original-words entry)
			   do (write-string word stream)
			   do (write-char #\Space stream)))
		do (terpri stream)
		do (loop for subentry in (subentries entry)
			 do (format stream " ~a~%" (name (category subentry)))
			 do (loop for meaning in (meanings subentry)
				  do (show-meaning meaning))
			 do (unless (null (synonym subentry))
			      (format stream "  Synonyms:~%")
			      (loop for meaning in (meanings (synonym subentry))
				    do (show-meaning meaning))))))))))

(define-presentation-method highlight-presentation :after
    ((type word-entry) record stream (state (eql :highlight)))
    (declare (ignore stream))
    (show-entry (presentation-object record)))

(define-transclime-command com-toggle-fix-entry ((object t))
  (if (fix-entry *application-frame*)
      (setf (fix-entry *application-frame*) nil)
      (progn (setf (fix-entry *application-frame*) t)
	     (show-entry object))))

(define-transclime-command (com-toggle-soft-match :name t) ()
  (setf (soft-match *application-frame*)
	(not (soft-match *application-frame*))))

(define-presentation-method highlight-presentation :before
    ((type word-entry) record stream (state (eql :unhighlight)))
    (declare (ignore stream state))
    (unless (fix-entry *application-frame*)
      (let ((stream (find-pane-named *application-frame* 'dico )))
	(window-clear stream))))

(define-presentation-to-command-translator toggle-fix-entry
    (word-entry com-toggle-fix-entry transclime)
  (object)
  `(,object))

(define-transclime-command (com-english-vietnamese :name t) ((word 'string))
  (flet ((word-is-in-entry-p (entry)
	   (loop for subentry in (subentries entry)
		 do (loop for meaning in (meanings subentry)
			  do (when (search word (translation meaning))
			       (return-from word-is-in-entry-p t))))))
    (let* ((stream (find-pane-named *application-frame* 'dico ))
	   (medium (sheet-medium stream))
	   (text-style (make-text-style :sans-serif :roman nil)))
      (window-clear stream)
      (with-text-style (medium text-style)
	(flet ((show-meaning (meaning)
		 (with-drawing-options (medium :ink +red+)
		   (format stream "   ~a~%" (translation meaning)))
		 (unless (null (examples meaning))
		   (format stream "     Examples:~%"))
		 (loop for example in (examples meaning)
		       do (format stream "      ~a~%" (original example))
		       do (unless (null (translation example))
			    (with-drawing-options (medium :ink +orange+)
			      (format stream "      ~a~%" (translation example)))))))
	  (loop for entry in *dico*
		do (when (word-is-in-entry-p entry)
		     (with-text-face (medium :bold)
		       (loop for word in (original-words entry)
			     do (write-string word stream)
			     do (write-char #\Space stream)))
		     (terpri stream)
		     (loop for subentry in (subentries entry)
			   do (format stream " ~a~%" (name (category subentry)))
			   do (loop for meaning in (meanings subentry)
				    do (show-meaning meaning))
			   do (unless (null (synonym subentry))
				(format stream "  Synonyms:~%")
				(loop for meaning in (meanings (synonym subentry))
				      do (show-meaning meaning)))))))))))
