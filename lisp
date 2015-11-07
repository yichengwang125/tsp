(setf (get 'a 'neighbors) '(b c d e)
      (get 'b 'neighbors) '(a c d e)
      (get 'c 'neighbors) '(a b d e)
      (get 'd 'neighbors) '(a b c e)
      (get 'e 'neighbors) '(a b c d))
      

(setf (get 'a 'distance) '((b 7) (c 6) (d 10) (e 13))
      (get 'b 'distance) '((a 7) (c 7) (d 10) (e 10))
      (get 'c 'distance) '((a 6) (b 7) (d 5)  (e 9))
      (get 'd 'distance) '((a 10) (b 10) (c 5) (e 6))
      (get 'e 'distance) '((a 13) (b 10) (c 9) (d 6)))
       
 (defun distance (node1 node2 &optional (list (get node1 'distance)))
(cond 
((null (first list)) nil)
((equal node2 (first (first list))) (second (first list)) )
      ('else (distance node1 node2 (rest list)))))
      
(defun path-length (path)
(cond 
((null (rest path)) 0)
('else (+ (distance (first path) (second path))(path-length (rest path))))))

(defun shorterp (path1 path2)
  (< (path-length path1) (path-length path2)))
  
(defun extend-path (path)
   (mapcar #'(lambda (new-node) (cons new-node path))
	  (remove-if 
	   #'(lambda (neighbor) (member neighbor path))
	   (get (first path) 'neighbors))))
	   
 (defun branch-and-bound (start  &optional (queue (list (list start))))
  (cond 
  ((endp queue) nil)
	((equal 6 (length (first queue)))
	 (reverse (first queue)))
	((equal 5 (length (first queue)))
	   ( branch-and-bound  start    (sort (append (cons start (first queue))
			(rest queue))
		#'(lambda (p1 p2) (shorterp p1 p2)))            )  )
	 ('else
	 (print queue)
	 	 (branch-and-bound	  start	   (sort (append (extend-path (first queue))
			(rest queue))
		#'(lambda (p1 p2) (shorterp p1 p2)))))
		))
