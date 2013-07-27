(setq None py:None)
(setq True py:True)
(setq False py:False)

(fsetq + (python "lambda x, y: x + y"))
(fsetq - (python "lambda x, y: x - y"))
(fsetq * (python "lambda x, y: x * y"))
(fsetq / (python "lambda x, y: x / y"))
(fsetq < (python "lambda x, y: x < y"))
(fsetq <= (python "lambda x, y: x <= y"))
(fsetq > (python "lambda x, y: x > y"))
(fsetq >= (python "lambda x, y: x >= y"))
(fsetq = (python "lambda x, y: x == y"))
(fsetq != (python "lambda x, y: x != y"))
(fsetq logand (python "lambda x, y: x & y"))
(fsetq logior (python "lambda x, y: x | y"))
(fsetq logxor (python "lambda x, y: x ^ y"))
(fsetq ash (python "lambda x, y: x << y if y >= 0 else x >> -y"))
(defun print (x) (out (+ (str x) "\n")))
(fsetq list (python "lambda *args: list(args)"))
(fsetq aref (python "lambda v, i: v[i]"))
(fsetq set-aref (python "lambda v, i, x: (v.__setitem__(i, x), x)[1]"))
(fsetq funcall (python "lambda f, *args: f(*args)"))
(fsetq apply (python "lambda f, args: f(*args)"))
(fsetq range (python "lambda *args: list(range(*args))"))
(fsetq map (python "lambda *args: list(map(*args))"))
(defun first (x) (aref x 0))
(defun second (x) (aref x 1))
(fsetq rest (python "lambda x: x[1:]"))
(defun xlist (x) (apply #'list x))
(fsetq length py:len)

(defmacro dotimes (var+count *body)
  (list 'mapn
        (+ (list 'lambda (list (first var+count)))
           (xlist body))
        (second var+count)))

(defmacro dolist (var+list *body)
  (list 'mapl
        (+ (list 'lambda (list (first var+list)))
           (xlist body))
        (second var+list)))

(defmacro when (test *body)
  (list 'if test
        (+ (list 'progn) (xlist body))))

(defmacro let (bindings *body)
  (+ (list 'funcall
           (+ (list 'lambda (map #'first bindings))
              (xlist body)))
     (map #'second bindings)))

(defmacro and (*conds)
  (if (= (length conds) 1)
      (first conds)
      (list 'if (first conds)
            (+ (list 'and) (xlist (rest conds)))
            False)))

(print "PyLisp 0.002")

;(python "globals().__setitem__('debug', True)")
