A Lisp dialect compiler targeting Python

- A compile-only implementation
- Works with Python 2.x / 3.x and PyPy
- Macros
- Lisp-2
- NOT aiming at becoming a Common Lisp implementation

Example session:

    ~/checkout/pylisp/src$ python pylisp.py
    PyLisp 0.002
    > (defun square (x) (* x x))
    --> <function lambda at 0x94a7a74>
    > (map #'square (range 10))
    --> (0 1 4 9 16 25 36 49 64 81)
    > (defun adder (x) (lambda (y) (setq x (+ x y))))
    --> <function lambda at 0x94a7b1c>
    > (let ((a (adder 10)))
        (dotimes (i 5)
          (print (funcall a 3))))
    Segmentation fault (core dumped)
    ~/checkout/pylisp/src$ :-(
