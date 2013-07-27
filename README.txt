A Lisp dialect compiler targeting Python bytecode

- A compile-only implementation
- Works with Python 2.x / 3.x and PyPy
- Macros
- Lisp-2
- NOT aiming at becoming a Common Lisp implementation

Example session:

    ~/checkout/pylisp/src$ python pylisp.py 
    PyLisp 0.002
    > (defun square (x) (* x x))
    --> <function lambda at 0x8f7abc4>
    > (map #'square (range 10))
    --> (0 1 4 9 16 25 36 49 64 81)
    > (defun adder (x) (lambda (y) (setq x (+ x y))))
    --> <function lambda at 0x8f7ac6c>
    > (let ((a (adder 10)))
        (dotimes (i 5)
          (print (funcall a 3))))
    13
    16
    19
    22
    25
    --> None
    > 
