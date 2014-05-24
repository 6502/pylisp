(setq None py:None)
(setq True py:True)
(setq False py:False)

(fsetq + (python "lambda x, *args: x+''.join(args) if isinstance(x, str) else sum(args, x)"))
(fsetq - (python "lambda x, *others: x - sum(others) if others else -x"))
(fsetq * (python "lambda x, y: x * y"))
(fsetq % (python "lambda x, y: x % y"))
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
(fsetq symbol-function (python "lambda x: globals().get('f' + x.name)"))
(fsetq symbol-macro (python "lambda x: globals().get('m' + x.name)"))
(defun first (x) (aref x 0))
(defun second (x) (aref x 1))
(defun last (x) (aref x -1))
(fsetq push (python "lambda x, L: (L.append(x), x)[1]"))
(fsetq rest (python "lambda x: x[1:]"))
(defun xlist (x) (apply #'list x))
(fsetq length py:len)
(fsetq slice (python "lambda L, a=0, b=None: L[a:b]"))

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

(defmacro not (x)
  (list 'bytecode
        x
        '(emit "UNARY_NOT")))

(defmacro unless (test *body)
  (list 'if (list 'not test)
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
            (+ (list 'progn) (rest (first cases)))
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

(defmacro aref (x *others)
  (if others
      (lassoc-binop "BINARY_SUBSCR" x others)
      x))

(fsetq butlast (python "lambda L: L[:-1]"))

(defmacro set-aref (x *others)
  (list 'bytecode
        (+ (list 'aref x) (xlist (butlast (butlast others))))
        (aref others (- (length others) 2))
        (last others)
        '(emit "DUP_TOP")
        '(stack-effect 1)
        '(emit "ROT_FOUR")
        '(emit "ROT_FOUR")
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
              ((and (list? el) el (= (aref el 0) '|,@|))
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

(setf *gensym* 0)

(defun gensym ()
  (setf *gensym* (+ 1 *gensym*))
  (intern (+ "#:" (str *gensym*) "")))

(defun macroexpand-1 (x)
  (if (and (list? x)
           x
           (symbol? (first x))
           (symbol-macro (first x)))
      (apply (symbol-macro (first x)) (rest x))
      x))

(fsetq dis (python "lambda x: dis.dis(x)"))

(defmacro funcall (f *args)
  `(bytecode
    ,f
    ,@(xlist args)
    (emit "CALL_FUNCTION" ,(length args))
    (stack-effect ,(- (length args)))))

(defmacro flet (bindings *body)
  `(funcall (lambda ,(map (lambda (n)
                            (intern (+ "py:f" (mangle (symbol-name (first n))))))
                      bindings)
              ,@(xlist body))
            ,@(map (lambda (n)
                     `(lambda ,@(rest n)))
                   bindings)))

(defmacro labels (bindings *body)
  `(funcall (lambda ,(map (lambda (n)
                            (intern (+ "py:f" (mangle (symbol-name (first n))))))
                      bindings)
              ,@(map (lambda (n)
                       `(setq ,(intern (+ "py:f" (mangle (symbol-name (first n)))))
                              (lambda ,@(rest n))))
                     bindings)
              ,@(xlist body))
            ,@(* (list None) (length bindings))))

(fsetq clock (python "__import__('time').time"))

(defmacro time (*body)
  (let ((start (gensym))
        (res (gensym)))
    `(let ((,start (clock))
           (,res (progn ,@(xlist body))))
       (print (+ (% "Time = %0.3f ms" (* 1000 (- (clock) ,start)))))
       ,res)))

(defmacro while (test *body)
  (if body
      (let ((testl (label))
            (loopl (label)))
        `(bytecode
          (emit "JUMP_ABSOLUTE" ,testl)
          (emit "LABEL" ,loopl)
          (progn ,@(xlist body))
          (emit "POP_TOP")
          (stack-effect -1)
          (emit "LABEL" ,testl)
          ,test
          (emit "POP_JUMP_IF_TRUE" ,loopl)
          ,None))
      (let ((loopl (label)))
        `(bytecode
          (emit "LABEL" ,loopl)
          ,test
          (emit "POP_JUMP_IF_TRUE" ,loopl)
          ,None))))

(defmacro push (x L)
  `(bytecode
    ,x
    (emit "DUP_TOP")
    (stack-effect 1)
    ,L
    (emit "LOAD_ATTR" "append")
    (emit "ROT_TWO")
    (emit "CALL_FUNCTION" 1)
    (emit "POP_TOP")
    (stack-effect -1)))

(defun modify-aref (place modifier)
  `(bytecode
    (aref ,@(butlast place))  ;; Array
    (emit "DUP_TOP")          ;; Array Array
    (stack-effect 1)
    ,(last place)             ;; Array Array Index
    (emit "DUP_TOP")          ;; Array Array Index Index
    (stack-effect 1)
    (emit "ROT_THREE")        ;; Array Index Array Index
    (emit "BINARY_SUBSCR")    ;; Array Index OldValue
    (stack-effect -1)
    ,@modifier                ;; Array Index NewValue
    (emit "DUP_TOP")          ;; Array Index NewValue NewValue
    (stack-effect 1)
    (emit "ROT_FOUR")         ;; NewValue Array Index NewValue
    (emit "ROT_FOUR")         ;; NewValue NewValue Array Index
    (emit "STORE_SUBSCR")     ;; NewValue
    (stack-effect -2)))

(defmacro inc-aref (*args)
  (modify-aref (xlist (butlast args))
               `(,(last args)
                 (emit "BINARY_ADD")
                 (stack-effect -1))))

(defmacro dec-aref (*args)
  (modify-aref (xlist (butlast args))
               `(,(last args)
                 (emit "BINARY_SUBTRACT")
                 (stack-effect -1))))

(defmacro incf (place *delta)
  (setf delta (if delta (first delta) 1))
  (cond
   ((symbol? place)
    `(setf ,place (+ ,place ,delta)))
   ((and (list? place)
         place
         (symbol? (first place)))
    `(,(intern (+ "inc-" (symbol-name (first place))))
      ,@(rest place)
      ,delta))
   (True (error "Invalid incf place"))))

(defmacro decf (place *delta)
  (setf delta (if delta (first delta) 1))
  (cond
   ((symbol? place)
    `(setf ,place (- ,place ,delta)))
   ((and (list? place)
         place
         (symbol? (first place)))
    `(,(intern (+ "dec-" (symbol-name (first place))))
      ,@(rest place)
      ,delta))
   (True (error "Invalid decf place"))))

(defmacro dotimes* (var+count *body)
  (let ((var (first var+count))
        (count (gensym)))
    `(let ((,var 0)
           (,count ,(second var+count)))
       (while (< ,var ,count)
         ,@(xlist body)
         (incf ,var)))))

(defmacro let** (bindings *body)
  (let ((symbols (map (lambda (b)
                        (if (and (list? (first b))
                                 (= (length (first b)) 2)
                                 (= 'function (first (first b))))
                            (intern (+ "py:f" (mangle (symbol-name (second (first b))))))
                            (first b)))
                      bindings)))
    `(let ,(map (lambda (s) `(,s None)) symbols)
       ,@(let ((res (list)))
           (dotimes* (i (length symbols))
             (push `(setf ,(aref symbols i)
                          ,(if (list? (first (aref bindings i)))
                               `(lambda ,@(rest (aref bindings i)))
                               (second (aref bindings i))))
                   res))
           res)
       ,@(xlist body))))

(defmacro tuple (*args)
  `(bytecode
    ,@(xlist args)
    (stack-effect ,(length args))
    (emit "BUILD_TUPLE" ,(length args))
    (stack-effect ,(- (length args)))
    (stack-effect 1)))

(print "PyLisp 0.006")
