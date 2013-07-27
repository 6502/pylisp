(setq None py:None)
(setq True py:True)
(setq False py:False)

(fsetq + (python "lambda x, *args: x+''.join(args) if isinstance(x, str) else sum(args, x)"))
(fsetq - (python "lambda x, *others: x - sum(others) if others else -x"))
(fsetq * (python "lambda x, y: x * y"))
(fsetq / (python "lambda x, y: x / y"))
(fsetq < (python "lambda *args: all(args[i-1] < args[i] for i in range(1, len(args)))"))
(fsetq <= (python "lambda *args: all(args[i-1] <= args[i] for i in range(1, len(args)))"))
(fsetq > (python "lambda *args: all(args[i-1] > args[i] for i in range(1, len(args)))"))
(fsetq >= (python "lambda *args: all(args[i-1] >= args[i] for i in range(1, len(args)))"))
(fsetq = (python "lambda *args: all(args[i-1] == args[i] for i in range(1, len(args)))"))
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
(fsetq push (python "lambda x, L: (L.append(x), x)[1]"))
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

(defun lassoc-binop (opcode x others)
  (let ((code (list 'bytecode x))
        (op (list 'emit opcode)))
    (dolist (y others)
      (push y code)
      (push op code)
      (push '(stack-effect -1) code))
    code))

(defmacro + (x *others) (if others (lassoc-binop "BINARY_ADD" x others) x))
(defmacro * (x *others) (if others (lassoc-binop "BINARY_MULTIPLY" x others) x))

(defmacro - (x *others)
  (if others
      (lassoc-binop "BINARY_SUBTRACT" x others)
      (list 'bytecode
            x
            '(emit "UNARY_NEGATIVE"))))

(defmacro / (x *others)
  (if others
      (lassoc-binop "BINARY_TRUE_DIVIDE" x others)
      (list 'bytecode
            1
            x
            '(emit "BINARY_TRUE_DIVIDE")
            '(stack-effect -1))))

(defun compare-op (name index x others)
  (if (= (length others) 1)
      (list 'bytecode
            x
            (first others)
            (list 'emit "COMPARE_OP" index)
            '(stack-effect -1))
      (+ (list 'funcall (list 'function name) x) (xlist others))))

(defmacro <  (x *others) (compare-op '<  0 x others))
(defmacro <= (x *others) (compare-op '<= 1 x others))
(defmacro =  (x *others) (compare-op '=  2 x others))
(defmacro /= (x *others) (compare-op '/= 3 x others))
(defmacro >  (x *others) (compare-op '>  4 x others))
(defmacro >= (x *others) (compare-op '>= 5 x others))

(print "PyLisp 0.003")

;(python "globals().__setitem__('debug', True)")
