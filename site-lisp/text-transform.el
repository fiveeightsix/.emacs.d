(defvar title-case-exclude '("at" "or" "but" "by" "for" "from" "in" "into" 
				   "like" "near" "of" "off" "on" "onto" "out" 
				   "over" "to" "up" "upon" "with" "nor" "so" 
				   "yet" "the" "if" "and")
  "List of words not to capitalize when in titles.")


(defun in-list-p (object list)
  "Returns t if supplied object is equal to one or more values in the given list."
  (if (equal object (car list))
      t
    (if (not (equal nil (cdr list)))
	(in-list-p object (cdr list))
      nil)))


(defun title-case-region (r-beg r-end)
  "Capitalize important words in the selected region, like a title."
  (interactive "r")
  (let (word 
	(count 0)) ; keep track of number of words
    (save-excursion
      (save-restriction
	(narrow-to-region r-beg r-end)
	;; Make everything lowercase, or matching won't work:
	(downcase-region r-beg r-end)
	(goto-char (point-min))
	;; Isolate words, work on one at a time:
	(while (re-search-forward "\\w\\{2,\\}" nil t)
	  (setq word (match-string 0)) 
	  (delete-region (match-beginning 0) (match-end 0))
	  ;; Capitalize word only if it's the first, or if it's not in the list:
	  (if (or (zerop count)
		  (not (in-list-p word title-case-exclude)))
	      (insert (capitalize word))
	    (insert word))
	  (setq count (1+ count)))))))


(defun title-case-string (t-str)
  "Capitalize important words in string, like a title."
  (with-temp-buffer
    (goto-char (point-min))
    (insert t-str)
    (title-case-region (point-min) (point-max))
    (buffer-string)))


(defun format-as-identifier (ws-str &optional separator rep-regexp)
  "Replace whitespace and punctuation in the given string with a separator."
  (interactive)
  ;; set default values if no optinal arguments given
  (if (not rep-regexp)
      (setq rep-regexp "[^A-Za-z0-9-]+"))
  (if (not separator) 
      (setq separator "-"))
  ;; trim ends
  (setq ws-str (replace-regexp-in-string "^[^A-Za-z0-9]+" "" ws-str))
  (setq ws-str (replace-regexp-in-string "[^A-Za-z0-9]+$" "" ws-str))
  ;; replace unwanted characters
  (setq ws-str (replace-regexp-in-string rep-regexp separator ws-str))
  ;; return string with no caps
  (downcase ws-str))





      
  