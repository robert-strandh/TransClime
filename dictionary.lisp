(cl:in-package #:transclime)

(defclass category ()
  ((%name :initarg :name :reader name)))

(defgeneric category-p (object)
  (:method (object) nil)
  (:method ((object category)) t))

(defclass example ()
  ((%original :initarg :original :reader original)
   (%translation :initform nil :initarg :translation :reader translation)))

(defgeneric example-p (object)
  (:method (object) nil)
  (:method ((object example)) t))

(defclass meaning ()
  ((%translation :initarg :translation :reader translation)
   (%examples :initform '() :initarg :examples :accessor examples)))

(defgeneric meaning-p (object))

(defmethod meaning-p (object)
  nil)

(defmethod meaning-p ((object meaning))
  t)

(defclass synonym ()
  ((%meanings :initform '() :initarg :meanings :accessor meanings)))

(defgeneric synonym-p (object))

(defmethod synonym-p (object)
  nil)

(defmethod synonym-p ((object synonym))
  t)

(defclass subentry ()
  ((%category :initform (make-instance 'category :name "unknown category")
	      :initarg :category :reader category)
   (%meanings :initarg :meanings :accessor meanings)
   (%synonym :initform nil :initarg :synonym :accessor synonym)))

(defgeneric subentry-p (object))

(defmethod subentry-p (object)
  nil)

(defmethod subentry-p ((object subentry))
  t)

(defclass entry ()
  ((%original-words :initarg :original-words :reader original-words)
   (%subentries :initform '() :initarg :subentries :accessor subentries)))

(defgeneric entry-p (object))

(defmethod entry-p (object)
  nil)

(defmethod entry-p ((object entry))
  t)

(defun create-meaning (lines)
  (setf (examples (first lines))
	(rest lines))
  (first lines))

(defun create-meanings (lines)
  (let ((pos (position-if #'meaning-p lines :start 1)))
    (if (null pos)
	(list (create-meaning lines))
	(cons (create-meaning (subseq lines 0 pos))
	      (create-meanings (nthcdr pos lines))))))

(defun create-synonym (lines)
  (setf (meanings (first lines))
	(create-meanings (rest lines)))
  (first lines))

(defun create-subentry (lines)
  (let ((pos (position-if #'synonym-p lines)))
    (if (null pos)
	(make-instance 'subentry
		       :category (first lines)
		       :meanings (create-meanings (rest lines)))
	(make-instance 'subentry
		       :category (first lines)
		       :meanings (create-meanings (subseq lines 1 pos))
		       :synonym (create-synonym (nthcdr pos lines))))))

(defun create-subentries (lines)
  (let ((pos (position-if #'category-p lines :start 1)))
    (if (null pos)
	(list (create-subentry lines))
	(cons (create-subentry (subseq lines 0 pos))
	      (create-subentries (nthcdr pos lines))))))

(defun create-entry (lines)
  (let ((remaining-lines
	 (if (category-p (second lines))
	     (rest lines)
	     (cons (make-instance 'category :name "unknown category") (rest lines)))))
    (setf (subentries (first lines))
	  (create-subentries remaining-lines))
    (first lines)))

(defun create-entries (lines)
  (loop for pos = (position-if #'entry-p lines :start 1)
	when (null pos) collect (create-entry lines)
	until (null pos)
	collect (create-entry (subseq lines 0 pos))
	do (setf lines (nthcdr pos lines))))

(defun parse-line (char line)
  (ecase char
    (#\@ (make-instance 'entry
			:original-words
			(split-sequence:split-sequence
			 #\Space line 
			 :remove-empty-subseqs t)))
    (#\- (make-instance 'meaning
			:translation line))
    (#\= (let ((pos (position #\+ line)))
	   (if (null pos)
	       (make-instance 'example :original line)
	       (make-instance 'example
			      :original (subseq line 0 pos)
			      :translation (subseq line (1+ pos))))))
    (#\* (make-instance 'category :name line))
    (#\# (make-instance 'synonym))))

(defun parse-lines (stream)
  (loop for char = (read-char-wrapped stream)
	until (null char)
	if (member char '(#\@ #\- #\= #\* #\#))
	collect (parse-line char (read-line-wrapped stream))
	else do (progn (unread-char-wrapped char stream) (read-line-wrapped stream))))


(defun entry-less (entry1 entry2)
  (labels ((words-less (words1 words2)
	     (cond ((null words1) t)
		   ((null words2) nil)
		   ((string-lessp (first words1) (first words2)) t)
		   ((string-lessp (first words2) (first words1)) nil)
		   (t (words-less (rest words1) (rest words2))))))
    (words-less (original-words entry1) (original-words entry2))))

(defun entry-equal (entry1 entry2)
  (equal (original-words entry1) (original-words entry2)))

(defun merge-dicos (entries1 entries2)
  (loop until (and (null entries1) (null entries2))
	collect (cond ((null entries1) (pop entries2))
		      ((null entries2) (pop entries1))
		      ((entry-equal (first entries1) (first entries2))
		       (pop entries1)
		       (pop entries2))
		      ((entry-less (first entries1) (first entries2))
		       (pop entries1))
		      (t (pop entries2)))))

(defun read-dico (stream)
  (sort (create-entries (parse-lines stream)) #'entry-less))
