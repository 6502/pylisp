import re
import sys

def f_Lout(x):
    sys.stdout.write(x)

def f_Lmangle(x):
    return "_L" + re.sub("[^A-Za-z]", lambda x: "_%02x" % ord(x.group(0)), x)

def f_Ldemangle(x):
    return re.sub("_([0-9a-f]{2})", lambda x: chr(int(x.group(1), 16)), x[2:])

indent = 0

def f_Lexpand(code):
    def expa(m):
        global indent
        x = m.group(1)[1]
        if x == "@":
            return "@"
        if x == "+":
            indent += 1
        elif x == "-":
            indent -= 1
        return "\n" + ("    " * indent)
    return re.sub("(@[-+=@])", expa, code)

class Symbol(object):
    def __init__(self, name):
        self.name = f_Lmangle(name)

    def __repr__(self):
        return f_Ldemangle(self.name)

symbols = {}

def f_Lintern(x):
    try:
        _ =symbols[x]
    except KeyError:
        _ = symbols[x] = Symbol(x)
    return _

_if = f_Lintern("if")
_progn = f_Lintern("progn")
_lambda = f_Lintern("lambda")
_setq = f_Lintern("setq")
_define = f_Lintern("define")
_defun = f_Lintern("defun")
_defmacro = f_Lintern("defmacro")
_quote = f_Lintern("quote")
_bquote = f_Lintern("`")
_commasplice = f_Lintern(",@")
_comma = f_Lintern(",")
_function = f_Lintern("function")
_python = f_Lintern("python")
_globals = []

_un = [0]

def f_Lcompile(x):
    if x is None or isinstance(x, (str, int, long, float, bool)):
        return "@=_ = %r" % x
    if isinstance(x, Symbol):
        if x.name[:7] == "_Lpy_3a":
            return "@=_ = %s" % f_Ldemangle(x.name)[3:]
        else:
            return "@=_ = %s[0]" % x.name
    if isinstance(x, list):
        macro = globals().get("m" + x[0].name)
        if macro:
            return f_Lcompile(macro(*x[1:]))

        if x[0] is _if:
            return (f_Lcompile(x[1]) +
                    "@=if _:@+" +
                    f_Lcompile(x[2]) +
                    "@-else:@+" +
                    f_Lcompile(x[3])
                    + "@-")

        if x[0] is _progn:
            if len(x) == 1:
                return "@=_ = None"
            else:
                return "@=" + "@=".join(map(f_Lcompile, x[1:]))

        if x[0] is _setq:
            return f_Lcompile(x[2]) + "@=%s[0] = _" % x[1].name

        if x[0] is _define:
            return f_Lcompile(x[2]) + "@=%s = [_]" % x[1].name

        if x[0] is _defun:
            return (f_Lcompile([_lambda] + x[2:]) +
                    "@=_.__name__ = %r" % f_Ldemangle(x[1].name) +
                    "@=f%s = _" % x[1].name)

        if x[0] is _defmacro:
            return (f_Lcompile([_lambda] + x[2:]) +
                    "@=_.__name__ = %r" % f_Ldemangle(x[1].name) +
                    "@=m%s = _" % x[1].name)

        if x[0] is _quote:
            _globals.append(x[1])
            return "@=_ = _globals[%i]" % (len(_globals) - 1)

        if x[0] is _python:
            return x[1]

        if x[0] is _lambda:
            def starclean(x):
                return "_L" + x[5:] if x[:5] == "_L_2a" else x

            def starclean2(x):
                return "list(_L" + x[5:] + ")" if x[:5] == "_L_2a" else x

            def stararg(x):
                return "*_L" + x[5:] if x[:5] == "_L_2a" else x

            return ("@=def _(" +
                    ", ".join(map(lambda y:stararg(y.name), x[1])) +
                    "):@+" +
                    "".join(map(lambda y: "@=%s = [%s]" %
                                (starclean(y.name),
                                 starclean2(y.name)),
                                x[1])) +
                    "@=" + f_Lcompile([_progn] + x[2:]) +
                    "@=return _@-")

        _un[0] += 1
        aid = _un[0]
        res = "@=_%i = []" % aid
        for y in x[1:]:
            res += "@=" + f_Lcompile(y) + "@=_%i.append(_)" % aid
        if x[0].name[:7] == "_Lpy_3a":
            return res + "@=_ = %s(*_%i)" % (f_Ldemangle(x[0].name)[3:], aid)
        else:
            return res + "@=_ = f%s(*_%i)" % (x[0].name, aid)
    raise RuntimeError("Unable to compile %r" % x)

def tokenize(x):
    tokens = re.findall("[ \\t\\n]+|" +                                   # spaces
                        "\\|(?:[^\\|\\\\]|\\\\.)*\\||" +                  # escaped symbol
                        ";.*|" +                                          # comment
                        "'|#'|`|,@|,|" +                                  # shortcuts ` #' ` ,@ ,
                        "[()]|" +                                         # open-close parenthesis
                        "\"(?:[^\"\\\\]|\\\\.)*\"|" +                     # quoted string
                        "[+-]?[0-9]+(?:\\.[0-9]+)(?:[eE]-?[0-9]+)?|" +    # number
                        "[^() \\n]*",                                     # symbol
                        x)
    tokens = [x for x in tokens if x.strip() != "" and x[0] != ";"][::-1]
    return tokens

def parse(x, f):
    tokens = tokenize(x)
    def r():
        v = tokens.pop()
        if v[0] == "\"":
            return eval(v)
        if v == "(":
            L = []
            while tokens[-1] != ")":
                L.append(r())
            tokens.pop()
            return L
        if v[0] == "|":
            return f_Lintern(v[1:-1])
        if v == "'":
            return [_quote, r()]
        if v == "`":
            return [_bquote, r()]
        if v == ",@":
            return [_commasplice, r()]
        if v == ",":
            return [_comma, r()]
        if v == "#'":
            return [_function, r()]
        try:
            return int(v)
        except ValueError:
            try:
                return float(v)
            except ValueError:
                return f_Lintern(v)
    while tokens:
        f(r())

def f_Loptimize(x):
    x = re.sub("(@=)+", "@=", x)
    while True:
        y = re.sub("@=(_[0-9]+) = \\[([^@]*)\\]" +
                   "@=_ = ([^@]*)" +
                   "@=\\1.append\\(_\\)",
                   lambda m: ("@=" +
                              m.group(1) +
                              " = [" +
                              m.group(2) +
                              (", " if m.group(2) else "") +
                              m.group(3) + "]"),
                   x)
        if y != x:
            x = y
        else:
            y = re.sub("@=(_[0-9]+) = \\[([^@]*)\\]" +
                       "@=_ = ([^@]*)\\(\\*\\1\\)",
                       lambda m: ("@=_ = " +
                                  m.group(3) +
                                  "(" +
                                  m.group(2) +
                                  ")"),
                       x)
            if y != x:
                x = y
            else:
                break
    return x

_L_2ashow_2dcode_2a = [False]

def f_Leval(x):
    code = f_Lcompile(x)
    code = f_Loptimize(code)
    code = f_Lexpand(code)
    if _L_2ashow_2dcode_2a[0]:
        print code
    exec(code, globals())
    return _

def f_Lstr(x):
    if isinstance(x, list):
        return "(" + " ".join(map(f_Lstr, x)) + ")"
    else:
        return str(x)

def f_Leval_2bprint(x):
    f_Lout("--> " + f_Lstr(f_Leval(x)) + "\n")

try:
    raw_input
except NameError:
    long = int
    raw_input = input

with open("pylisp.lisp") as f:
    parse(f.read(), f_Leval)

def balanced(x):
    tk = tokenize(x)
    return x.count("(") == x.count(")")

curr = ""
while True:
    t = raw_input("> " if balanced(curr) else "  ")
    curr += t + "\n"
    if balanced(curr):
        parse(curr, f_Leval_2bprint)
        curr = ""
