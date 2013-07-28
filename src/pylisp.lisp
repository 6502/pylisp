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
(fsetq symbol? (python "lambda x: isinstance(x, Symbol)"))
(fsetq list? (python "lambda x: isinstance(x, list)"))
(fsetq string? (python "lambda x: isinstance(x, (str, unicode))"))
(fsetq number? (python "lambda x: isinstance(x, (int, float, long))"))
(fsetq symbol-name (python "lambda x: f_Ldemangle(x.name)"))
(defun first (x) (aref x 0))
(defun second (x) (aref x 1))
(defun last (x) (aref x -1))
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

(defmacro and (x *conds)
  (let ((code (list 'bytecode x))
        (target (label)))
    (dolist (y conds)
      (push (list 'emit "JUMP_IF_FALSE_OR_POP" target) code)
      (push '(stack-effect -1) code)
      (push y code))
    (push (list 'emit "LABEL" target) code)
    code))

(defmacro or (x *conds)
  (let ((code (list 'bytecode x))
        (target (label)))
    (dolist (y conds)
      (push (list 'emit "JUMP_IF_TRUE_OR_POP" target) code)
      (push '(stack-effect -1) code)
      (push y code))
    (push (list 'emit "LABEL" target) code)
    code))

(defmacro cond (*cases)
  (if cases
      (list 'if
            (first (first cases))
            (second (first cases))
            (+ (list 'cond) (xlist (rest cases))))
      None))

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

(defmacro aref (x *others) (if others (lassoc-binop "BINARY_SUBSCR" x others) x))

(fsetq butlast (python "lambda L: L[:-1]"))

(defmacro set-aref (x *others)
  (list 'bytecode
        (last others)
        '(emit "DUP_TOP")
        '(stack-effect 1)
        (+ (list 'aref x) (xlist (butlast (butlast others))))
        (aref others (- (length others) 2))
        '(emit "STORE_SUBSCR")
        '(stack-effect -1)))

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

(defmacro setf (place value)
  (if (symbol? place)
      (list 'setq place value)
      (if (and (list? place) (symbol? (first place)))
          (+ (list (intern (+ "set-" (symbol-name (first place)))))
             (rest place)
             (list value))
          (error "Invalid setf place"))))

(defun bqconst (x)
  (if (and (list? x) x)
      (if (or (= (aref x 0) '|,|)
              (= (aref x 0) '|`|)
              (= (aref x 0) '|,@|))
          False
          (and (bqconst (first x))
               (bqconst (rest x))))
      True))

(defun bquote (x)
  (cond
    ((or (number? x) (string? x))
     x)
    ((bqconst x)
     (list 'quote x))
    ((list? x)
     (cond
       ((= (aref x 0) '|`|)
        (list '|`| (bquote (aref x 1))))
       ((= (aref x 0) '|,|)
        (aref x 1))
       ((= (aref x 0) '|,@|)
        (error ",@ must be used inside lists"))
       (True
        (let ((res (list '+))
              (clist (list 'list)))
          (dolist (el x)
            (cond
              ((and (list? el) (= (aref el 0) '|,@|))
               (when (> (length clist) 1)
                 (push clist res)
                 (setq clist (list 'list)))
               (push (aref el 1) res))
              (True
               (push (bquote el) clist))))
          (when (> (length clist) 1)
            (push clist res))
          (if (> (length res) 2)
              res
              (aref res 1))))))
    (True (list 'quote x))))

(defmacro |`| (x)
  (bquote x))

(print "PyLisp 0.004")
