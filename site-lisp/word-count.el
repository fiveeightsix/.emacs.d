;;;; Word counting functions

;;; Based on count-words-region from: 
;;; http://www.gnu.org/software/emacs/emacs-lisp-intro/html_node/Whitespace-Bug.html#Whitespace-Bug


(defun count-words (beginning end)
  "Returns the number of words in the region.
Use count-words-region to call interactively."
  (save-excursion
    (let ((count 0))
      (goto-char beginning)
      (while (and (< (point) end)
		  (re-search-forward "\\w+\\W*" end t))
	(setq count (1+ count)))
      count)))
    

(defun count-words-region (beginning end)
  "Print number of words in the region."
  (interactive "r")
  (message "Counting words in region ... ")
  (save-excursion
    (let (count)
      ;; get the word count:
      (setq count (count-words beginning end))
      ;; print in message:
      (cond ((zerop count)
	     (message "The region does NOT have any words."))
	    ((= 1 count)
	     (message "The region has 1 word."))
	    (t
	     (message "The region has %d words." count))))))


(defun count-words-xml (beginning end)
  "Returns the number of words in region, excluding XML tags.
Use count-words-region-xml to call interactively."
  (let ((oldbuf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring oldbuf beginning end)
      (goto-char (point-min))
      ;; remove all tags:
      (while (re-search-forward "</?[^\0]*?>" nil t)
	(replace-match "" nil nil))
      ;; count what remains:
      (count-words (point-min) (point-max)))))


(defun count-words-region-xml (beginning end)
  "Print number of words in region, excluding XML tags."
  (interactive "r")
  (save-excursion
    (let (count)
      ;; get the word count:
      (setq count (count-words-xml beginning end))
      ;; print in message:
      (cond ((zerop count)
	     (message "The region does NOT have any words."))
	    ((= 1 count)
	     (message "The region has 1 word, excluding XML tags."))
	    (t
	     (message "The region has %d words, excluding XML tags." count))))))

      