#||
Author: Delano Yoder
Date: 05/13/2021
||#

(require 'TilingWindows)

;;; Exercise 1

;;; Procedure tests.
(terpri)
(terpri)
(format t "~10@aExercise 1 Tests." "")
(terpri)

(setf *screen* (make_screen 200 100))

(terpri)
(format t "make_screen : (200, 100) -> ~a = ~a~%" (type-of *screen*) *screen*)
(format t "width : screen -> ~a = ~a~%" (type-of (width *screen*)) (width *screen*))
(format t "height : screen -> ~a = ~a~%" (type-of (height *screen*)) (height *screen*))
(format t "id : screen -> ~a = ~a~%" (type-of (id *screen*)) (id *screen*))
(terpri)

;;; Edge cases.
(format t "~10@aExercise 1 Edge Cases." "")
(terpri)

(terpri)
(format t "make_screen : (-1, 100) -> ")
(make_screen -1 100)
(format t "make_screen : (200, -1) -> ")
(make_screen 200 -1)
(format t "make_screen : ('a', 100) -> ")
(make_screen "a" 100)
(format t "make_screen : (200, 'a') -> ")
(make_screen 200 "a")
(terpri)

;;; Exercise 2 & 3

;;; Procedure tests.
(format t "~10@aExercise 2 & 3 Tests." "")
(terpri)

(terpri)
(setf *hsplit_list* (hsplit *screen* 75))
(setf *top* (car *hsplit_list*))
(setf *bottom* (cadr *hsplit_list*))

(format t "hsplit : (screen, 75) -> ~a = (~a, ~a)~%" (type-of *hsplit_list*) *top* *bottom*)
(terpri)

(format t "width : top split -> ~a = ~a~%" (type-of (width *top*)) (width *top*))
(format t "height : top split -> ~a = ~a~%" (type-of (height *top*)) (height *top*))
(format t "id : top split -> ~a = ~a~%" (type-of (id *top*)) (id *top*))
(terpri)

(format t "width : bottom split -> ~a = ~a~%" (type-of (width *bottom*)) (width *bottom*))
(format t "height : bottom split -> ~a = ~a~%" (type-of (height *bottom*)) (height *bottom*))
(format t "id : bottom split -> ~a = ~a~%" (type-of (id *bottom*)) (id *bottom*))
(terpri)

(setf *vsplit_list* (vsplit *top* 150))
(setf *left* (car *vsplit_list*))
(setf *right* (cadr *vsplit_list*))

(format t "vsplit : (top split, 150) -> ~a = (~a, ~a)~%" (type-of *vsplit_list*) *left* *right*)
(terpri)

(format t "width : left split -> ~a = ~a~%" (type-of (width *left*)) (width *left*))
(format t "height : left split -> ~a = ~a~%" (type-of (height *left*)) (height *left*))
(format t "id : left split -> ~a = ~a~%" (type-of (id *left*)) (id *left*))
(terpri)

(format t "width : right split -> ~a = ~a~%" (type-of (width *right*)) (width *right*))
(format t "height : right split -> ~a = ~a~%" (type-of (height *right*)) (height *right*))
(format t "id : right split -> ~a = ~a~%" (type-of (id *right*)) (id *right*))
(terpri)

(format t "active : screen -> ~a = ~a~%" (type-of (active *screen*)) (active *screen*))
(format t "passive : screen -> ~a = ~a~%" (type-of (passive *screen*)) (passive *screen*))
(terpri)

;;; Edge cases.
(format t "~10@aExercise 2 & 3 Edge Cases." "")
(terpri)

(terpri)
(format t "hsplit : (screen, 50) -> ")
(hsplit *screen* 50)
(format t "vsplit : (screen, 50) -> ")
(vsplit *screen* 50)
(format t "hsplit : (left split, 75) -> ")
(hsplit *left* 75)
(format t "hsplit : (left split, 150) -> ")
(vsplit *left* 150)
(format t "vsplit : (left split, 0) -> ")
(hsplit *left* 0)
(format t "vsplit : (left split, 0) -> ")
(vsplit *left* 0)
(format t "hsplit : (left split, 'a') -> ")
(hsplit *left* "a")
(format t "vsplit : (left split, 'a') -> ")
(vsplit *left* "a")
(terpri)

;;; Exercise 4

;;; Procedure tests.
(format t "~10@aExercise 4 Tests." "")
(terpri)

(terpri)
(format t "delete_viewport : (screen, 1) -> ~a~%" (type-of (_delete_ *screen* 1)))
(format t "id : screen -> ~a = ~a~%" (type-of (id *screen*)) (id *screen*))
(format t "id : top split -> ~a = ~a~%" (type-of (id *top*)) (id *top*))
(format t "id : bottom split -> ~a = ~a~%" (type-of (id *bottom*)) (id *bottom*))
(format t "id : left split -> ~a = ~a~%" (type-of (id *left*)) (id *left*))
(format t "id : right split -> ~a = ~a~%" (type-of (id *right*)) (id *right*))
(terpri)

(setf *hsplit_list* (hsplit *screen* 75))
(setf *top* (car *hsplit_list*))
(setf *bottom* (cadr *hsplit_list*))

(setf *vsplit_list* (vsplit *top* 150))
(setf *left* (car *vsplit_list*))
(setf *right* (cadr *vsplit_list*))

(format t "hsplit : (screen, 75) -> ~a = (~a, ~a)~%" (type-of *hsplit_list*) *top* *bottom*)
(format t "vsplit : (top split, 150) -> ~a = (~a, ~a)~%" (type-of *vsplit_list*) *left* *right*)
(terpri)

(format t "delete_viewport : (screen, 4) -> ~a~%" (type-of (_delete_ *screen* 4)))
(format t "id : screen -> ~a = ~a~%" (type-of (id *screen*)) (id *screen*))
(format t "id : top split -> ~a = ~a~%" (type-of (id *top*)) (id *top*))
(format t "id : bottom split -> ~a = ~a~%" (type-of (id *bottom*)) (id *bottom*))
(format t "id : left split -> ~a = ~a~%" (type-of (id *left*)) (id *left*))
(format t "id : right split -> ~a = ~a~%" (type-of (id *right*)) (id *right*))
(terpri)

;;; Edge cases.
(format t "~10@aExercise 4 Edge Cases." "")
(terpri)

(terpri)
(format t "delete_viewport : (screen, 6) -> ")
(_delete_ *screen* 6)
(format t "delete_viewport : (screen, 'a') -> ")
(_delete_ *screen* "a")
(terpri)

;;; Exercise 5

;;; Procedure tests.
(format t "~10@aExercise 5 Tests." "")
(terpri)

(resize *screen*  *top* 0.75)

(terpri)
(format t "width : top split -> ~a = ~a~%" (type-of (width *top*)) (width *top*))
(format t "height : top split -> ~a = ~a~%" (type-of (height *top*)) (height *top*))
(format t "id : top split -> ~a = ~a~%" (type-of (id *top*)) (id *top*))
(terpri)

;;; Edge cases.
(format t "~10@aExercise 5 Edge Cases." "")
(terpri)

(terpri)
(format t "resize : (screen, top split, 0) -> ")
(resize *screen* *top* 0)
(format t "resize : (screen, top split, 1) -> ")
(resize *screen* *top* 1)
(terpri)
(format t "resize : (screen, top split, -0.01) -> ")
(resize *screen* *top* -0.01)
(format t "resize : (screen, top split, 1.01) -> ")
(resize *screen* *top* 1.01)
(terpri)

;;; Exercise 6

;;; Procedure tests.
(format t "~10@aExercise 6 Tests." "")
(terpri)

(terpri)
(format t "width : bottom split -> ~a = ~a~%" (type-of (width *bottom*)) (width *bottom*))
(format t "height : bottom split -> ~a = ~a~%" (type-of (height *bottom*)) (height *bottom*))
(terpri)

(format t "select : (bottom split, (50, 80)) -> ~a = ~a~%" (type-of (select *top* '(50 80))) (select *top* '(50 80)))
(terpri)

;;; Edge cases.
(format t "~10@aExercise 6 Edge Cases." "")
(terpri)

(terpri)
(format t "select : (bottom split, (0, 20)) -> ")
(select *bottom* '(0 20))
(format t "select : (bottom split, (50, 0)) -> ")
(select *bottom* '(50 0))
(format t "select : (bottom split, (200, 20)) -> ")
(select *bottom* '(200 20))
(format t "select : (bottom split, (50, 25)) -> ")
(select *bottom* '(50 25))
(format t "select : (bottom split, ('a', 20)) -> ")
(select *bottom* '("a" 20))
(format t "select : (bottom split, (50, 'a')) -> ")
(select *bottom* '(50 "a"))
(terpri)