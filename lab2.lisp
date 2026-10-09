;; Лістинг функції 'remove-thirds-and-reverse'

(defun remove-thirds-and-reverse(lst)
	(cond
		((null lst) nil)
		((null (rest lst)) (list (first lst)))
		((null (rest (rest lst))) (list (second lst) (first lst)))
		(t (append (remove-thirds-and-reverse (rest(rest(rest lst)))) (list (second lst) (first lst))))
))

;; Тестові набори та утиліти
(defun check-remove-thirds-and-reverse (name input expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (remove-thirds-and-reverse input) expected)
          name))

(defun test-remove-thirds-and-reverse ()
  (check-remove-thirds-and-reverse "test 1" '(a b c d e f g) '(g e d b a))
  (check-remove-thirds-and-reverse "test 2" nil nil)
  (check-remove-thirds-and-reverse "test 3" '(1 2) '(2 1))
  (check-remove-thirds-and-reverse "test 4" '(1 2 3 4 5 6) '(5 4 2 1)))

;; Тестування
(test-remove-thirds-and-reverse)

;; Лістинг функції 'list-set-difference-3'
(defun list-set-difference-3(lst1 lst2 lst3)
	(cond 
		((null lst1) nil)
		((member (car lst1) lst2) (list-set-difference-3 (cdr lst1) lst2 lst3))
		((member (car lst1) lst3) (list-set-difference-3 (cdr lst1) lst2 lst3))
		(t (cons (car lst1) (list-set-difference-3 (cdr lst1) lst2 lst3)))
	))

;; Тестові набори та утиліти
(defun check-list-set-difference-3 (name lst1 lst2 lst3 expected)
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (list-set-difference-3 lst1 lst2 lst3) expected)
          name))

(defun test-list-set-difference-3 ()
  (check-list-set-difference-3 "test 1" '(1 2 3 4) '(4 5 6) '(2 5 7) '(1 3))
  (check-list-set-difference-3 "test 2" nil '(1 2) '(3 4) nil)
  (check-list-set-difference-3 "test 3" '(a b c) '(d e) '(f g) '(a b c))
  (check-list-set-difference-3 "test 4" '(1 2) '(1 5) '(2 6) nil))

;; Тестування
(test-list-set-difference-3)
