;;; do-lists --- apply function to each element of enclosed list in turn
;;;
;;; Copyright (C) 2017 Roman Tsykaliak
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or (at
;;; your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;
;;; Commentary:
;;; (do-lists 'min '((1 2 3) (2 3 4 5) (1 2)))
;;; (1 2 0 0)
;;; (do-lists 'max '((1 2 3) (2 3 4 5) (1 2)))
;;; (2 3 4 5)
;;; (do-lists '/ '((1 2 3) (2 3 4 5) (1 2)))
;;; (error)
;;; (do-lists '/ '((2 3 4 5) (1 2 1 2) (1 1 4 1)))
;;; (2 1 1 2)
;;; (do-lists '* '((1 2 3) (2 3 4 5) (1 2)))
;;; (2 12 0 0)
;;; (do-lists '+ '((1 2 3) (2 3 4 5) (1 2)))
;;; (4 7 7 5)
;;; (do-lists '- '((1 2 3) (2 3 4 5) (1 2)))
;;; (-2 -3 -1 -5)
;;; Does not work with '> or '<
;;;
;;; Code:
(defun do-lists (FUNCTION ARGUMENT)
  "Usage: (do-lists '* '((1 2 3) (2 3 4 5) (1 2))) -> (2 12 0 0).

Apply FUNCTION to each respective element of each list in ARGUMENT.
The first element of the first list and the first element of the second
list are operated on by a FUNCTION.  Then, the second element of the
first list and the second element of the second list are operated on
by a FUNCTION.  The process continues till it runs out of elements in
the first or second list.  After that it repeats the process, but now
operates on the first and third lists, if any.  That continues till it
runs out of lists.  The longest enclosed list in ARGUMENT defines the
length of the returned list.  Before the FUNCTION is applied to each
element of the current enclosed list, the list is extended by appending
0s to the end.  That is done only if lists length is less than the
length of the longest list.  It was done to preserve the order of
enclosed lists for division.  Was tested for these FUNCTIONs:
'min 'max '/ '* '+ '-."
  (let* ((len (apply 'max (mapcar 'length ARGUMENT)))
         (ret (append (car ARGUMENT)
                      (make-list (- len (length (car ARGUMENT))) 0))))
    (setq ARGUMENT (cdr ARGUMENT))
    (dolist (el ARGUMENT ret)
      (setq el (append el (make-list (- len (length el)) 0)))
      (dotimes (i len ret)
        (setf (nth i ret) (funcall FUNCTION (elt ret i) (elt el i)))))))

(provide 'do-lists)
;;; do-lists.el ends here
