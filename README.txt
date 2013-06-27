A Lisp dialect compiler targeting Python

- A compile-only implementation
- Works with Python 2.x / 3.x and PyPy
- Macros
- Lisp-2
- NOT aiming at becoming a Common Lisp implementation

Example session:

    ~/checkout/pylisp/src$ python pylisp.py
    PyLisp 0.001
    > (defun square (x) (* x x))
    --> <function square at 0xb744fbc4>
    > (map #'square (range 10))
    --> (0 1 4 9 16 25 36 49 64 81)
    > (setf *show-code* True)
    --> True
    > (defun fact (x) (if (< x 2) 1 (* x (fact (- x 1)))))

    def _(_Lx):

        _Lx = [_Lx]
        _ = f_L_3c(_Lx[0], 2)
        if _:

            _ = 1
        else:

            _ = f_L_2a(_Lx[0], f_Lfact(f_L_2d(_Lx[0], 1)))

        return _

    _.__name__ = 'fact'
    f_Lfact = _
    --> <function fact at 0xb744fcdc>
    >
