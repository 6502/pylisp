import re
import sys
import dis
import inspect
from opcode import opname, opmap
import types

# When true every code object produced is immediately disassembled
debug = False

# Map all defined opcodes to globals
for k in opmap.keys():
    globals()[k.replace("+", "_PLUS_")] = opmap[k]

# A few extra opcodes are defined as negative numbers and are used
# during first compilation pass.  Before creating the actual Code
# object they're resolved to final python bytecode

# Generic opcodes referring to globals, locals or refs
LOAD = -1
STORE = -2

# Load_closure opcode
LOADCL = -3

# Utility functions

def f_Lout(x):
    sys.stdout.write(x)

def f_Lmangle(x):
    return "_L" + re.sub("[^A-Za-z]", lambda x: "_%02x" % ord(x.group(0)), x)

def f_Ldemangle(x):
    return re.sub("_([0-9a-f]{2})", lambda x: chr(int(x.group(1), 16)), x[2:])

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

# Symbols known to the compiler

_if = f_Lintern("if")
_progn = f_Lintern("progn")
_lambda = f_Lintern("lambda")
_setq = f_Lintern("setq")
_define = f_Lintern("define")
_defun = f_Lintern("defun")
_defmacro = f_Lintern("defmacro")
_fsetq = f_Lintern("fsetq")
_msetq = f_Lintern("msetq")
_quote = f_Lintern("quote")
_bquote = f_Lintern("`")
_commasplice = f_Lintern(",@")
_comma = f_Lintern(",")
_function = f_Lintern("function")
_python = f_Lintern("python")
_emit = f_Lintern("emit")
_bytecode = f_Lintern("bytecode")
_stackeffect = f_Lintern("stack-effect")

# Global array for quoted values
_globals = []


# Compiler context object

ctx = None

class Context(object):
    def __init__(self):
        self.local = {}         # known local names
        self.outer = {}         # known names from outer scopes
        self.captured = {}      # local names captured by inner scopes

        self.pending_fixes = {}
        self.args = 0
        self.curstack = 1
        self.maxstack = 1
        self.constants = [None] # Why None is always here??
        self.locals = []        # Local names
        self.names = []         # Global names
        self.cellvars = []      # Names of locals captured
        self.freevars = []      # Nonlocals
        self.code = []

    def stack(self, *n):
        for delta in n:
            self.curstack += delta
            if self.curstack > self.maxstack:
                self.maxstack = self.curstack

    def absfix(self, id):
        f = ["abs", -1]
        try:
            self.pending_fixes[id].append(f)
        except KeyError:
            self.pending_fixes[id] = [f]
        return f

    def fix(self, id):
        self.pending_fixes[id].pop()[1] = len(self.code)

    def make_code(self, name, rest):
        ctx.code.append((RETURN_VALUE,))

        # Fix up generic opcodes
        free_fixup = []
        for i, op in enumerate(self.code):
            if op[0] in (LOAD, STORE, LOADCL):
                opcode, sym = op
                if sym in self.captured:
                    opcode = {LOAD: LOAD_DEREF,
                              STORE: STORE_DEREF,
                              LOADCL: LOAD_CLOSURE}[opcode]
                    if sym not in self.cellvars:
                        self.cellvars.append(sym)
                    sym = self.cellvars.index(sym)
                elif sym in self.local:
                    # Opcode cannot be LOADCL
                    opcode = {LOAD: LOAD_FAST,
                              STORE: STORE_FAST}[opcode]
                    if sym not in self.locals:
                        self.locals.append(sym)
                    sym = self.locals.index(sym)
                elif sym in self.outer:
                    opcode = {LOAD: LOAD_DEREF,
                              STORE: STORE_DEREF,
                              LOADCL: LOAD_CLOSURE}[opcode]
                    # Inform outer scope of the capture
                    self.outer[sym] += 1
                    if sym not in self.freevars:
                        self.freevars.append(sym)
                    # Index must be computed later because it's relative
                    # to the number of cellvars
                    free_fixup.append(i)
                else:
                    opcode = LOAD_GLOBAL if opcode is LOAD else STORE_GLOBAL
                    if sym not in self.names:
                        self.names.append(sym)
                    sym = self.names.index(sym)
                self.code[i] = (opcode, sym)
        for i in free_fixup:
            opcode, sym = self.code[i]
            self.code[i] = (opcode,
                            len(self.cellvars) + self.freevars.index(sym))
        bytestr = []

        pc = 0
        addr = []
        for op in self.code:
            addr.append(pc)
            pc += 1 if len(op) == 1 else 3

        for op in self.code:
            bytestr.append(op[0])
            if len(op) == 2:
                v = op[1]
                if isinstance(v, list):
                    v = addr[v[1]]
                bytestr.append(v & 255)
                bytestr.append(v >> 8)

        bytestr = bytes(bytestr) if sys.version_info > (3,) else "".join(map(chr, bytestr))

        flags = 0 if self.cellvars else (inspect.CO_NEWLOCALS | inspect.CO_OPTIMIZED)

        if not self.freevars:
            flags |= inspect.CO_NOFREE

        if rest:
            flags |= inspect.CO_VARARGS
            self.args -= 1

        co = types.CodeType(*([self.args,] +                             # argcount
                              ([0] if sys.version_info > (3,) else []) + # kw-only argcount
                              [len(self.locals),                         # nlocals
                               self.maxstack,                            # stacksize
                               flags,                                    # flags
                               bytestr,                                  # bytecode
                               tuple(self.constants),                    # constants
                               tuple(self.names),                        # names
                               tuple(self.locals),                       # varnames
                               "<bytecode>",                             # filename
                               name,                                     # name
                               0,                                        # firstlineno
                               bytes(),                                  # lnotab
                               tuple(self.freevars),                     # freevars
                               tuple(self.cellvars)]))                   # cellvars
        if debug:
            f_Lout("Created callable %s (stacksize = %i)\n" % (name, self.maxstack))
            f_Lout("  local = %r\n" % self.local)
            f_Lout("  outer = %r\n" % self.outer)
            f_Lout("  captured = %r\n" % self.captured)
            for k in dir(co):
                if k[:1] != "_":
                    f_Lout(" %s = %r\n" % (k, getattr(co, k)))
            dis.dis(co)
            f_Lout("-" * 70 + "\n")
        return co

# Compiles an expression to current compiler context
def f_Lcompile(x):
    global ctx
    if x is None or isinstance(x, (str, int, long, float, bool)):
        if x not in ctx.constants:
            ctx.constants.append(x)
        ctx.code.append((LOAD_CONST, ctx.constants.index(x)))
        ctx.stack(1)
        return

    if isinstance(x, Symbol):
        if x.name[:7] == "_Lpy_3a":
            ctx.names.append(f_Ldemangle(x.name)[3:])
            ctx.code.append((LOAD_GLOBAL, len(ctx.names)-1))
        else:
            ctx.code.append((LOAD, x.name))
        ctx.stack(1)
        return

    if isinstance(x, list):
        s = x[0]

        m = globals().get("m" + s.name)
        if m:
            f_Lcompile(m(*x[1:]))
            return

        if s is _emit:
            opcode = globals()[x[1]]
            ctx.code.append(tuple([opcode] + x[2:]))
            return

        if s is _bytecode:
            for y in x[1:]:
                f_Lcompile(y)
            return

        if s is _stackeffect:
            ctx.stack(*x[1:])
            return

        if s is _setq:
            f_Lcompile(x[2])
            ctx.code.append((DUP_TOP,))
            s = x[1]
            if s.name[:7] == "_Lpy_3a":
                ctx.names.append(f_Ldemangle(s.name)[3:])
                ctx.code.append((STORE_GLOBAL, len(ctx.names)-1))
            else:
                ctx.code.append((STORE, s.name))
            ctx.stack(2, -1)
            return

        if s is _progn:
            if len(x) == 1:
                f_Lcompile(None)
            else:
                f_Lcompile(x[1])
                for y in x[2:]:
                    ctx.code.append((POP_TOP,))
                    ctx.stack(-1)
                    f_Lcompile(y)
            return

        if s is _if:
            f_Lcompile(x[1])
            ctx.code.append((POP_JUMP_IF_FALSE, ctx.absfix(0)))
            ctx.stack(-1)
            f_Lcompile(x[2])
            ctx.code.append((JUMP_ABSOLUTE, ctx.absfix(1)))
            ctx.fix(0)
            f_Lcompile(x[3] if len(x) == 4 else None)
            ctx.fix(1)
            return

        if s is _quote:
            ctx.constants.append(x[1])
            ctx.code.append((LOAD_CONST, len(ctx.constants)-1))
            ctx.stack(1)
            return

        if s is _function:
            ctx.code.append((LOAD, "f" + x[1].name))
            ctx.stack(1)
            return

        if s is _lambda:
            octx = ctx
            ctx = Context()
            ctx.outer = octx.outer.copy()
            argnames = [v.name for v in x[1]]
            rest = False
            if argnames and argnames[-1][:5] == '_L_2a' and len(argnames[-1]) > 5:
                # Rest argument
                rest = True
                argnames[-1] = "_L" + argnames[-1][5:]
            for v in octx.local.keys():
                ctx.outer[v] = 0
            for v in argnames:
                ctx.local[v] = 0
                ctx.args += 1
                # The first `arg` names in locals are the parameters,
                # even if a parameter is captured in a closure; this
                # implies there is a negligible waste of one unused
                # slot for locals in the stack frame for each captured
                # parameter.
                ctx.locals.append(v)
            f_Lcompile([_progn] + x[2:])
            code = ctx.make_code("lambda", rest)
            for v in ctx.outer:
                if ctx.outer[v]:
                    if v in octx.local:
                        # A local in parent context has been captured
                        octx.captured[v] = 1
                    else:
                        # Propagate captures to upper levels
                        octx.outer[v] = 1
            nctx = ctx
            ctx = octx
            ctx.constants.append(code)
            if nctx.freevars:
                # Closure
                for v in nctx.freevars:
                    ctx.code.append((LOADCL, v))
                    ctx.stack(1)
                ctx.code.append((BUILD_TUPLE, len(nctx.freevars)))
                ctx.stack(-len(nctx.freevars), 1)
                ctx.code.append((LOAD_CONST, len(ctx.constants)-1))
                ctx.stack(1)
                if sys.version_info > (3,):
                    # Python 3 qualified name
                    ctx.constants.append("<lambda>")
                    ctx.code.append((LOAD_CONST, len(ctx.constants)-1))
                    ctx.stack(1, -3)
                else:
                    ctx.stack(-2)
                ctx.code.append((MAKE_CLOSURE, 0))
            else:
                # Function (why not an empty closure?)
                ctx.code.append((LOAD_CONST, len(ctx.constants)-1))
                ctx.stack(1)
                if sys.version_info > (3,):
                    # Python 3 qualified name
                    ctx.constants.append("<lambda>")
                    ctx.code.append((LOAD_CONST, len(ctx.constants)-1))
                    ctx.stack(1, -2)
                else:
                    ctx.stack(-1)
                ctx.code.append((MAKE_FUNCTION, 0))
            return

        if s is _fsetq:
            f_Lcompile([_setq, f_Lintern("py:f" + x[1].name), x[2]])
            return

        if s is _msetq:
            f_Lcompile([_setq, f_Lintern("py:m" + x[1].name), x[2]])
            return

        if s is _defun:
            f_Lcompile([_setq, f_Lintern("py:f" + x[1].name), [_lambda] + x[2:]])
            return

        if s is _defmacro:
            f_Lcompile([_setq, f_Lintern("py:m" + x[1].name), [_lambda] + x[2:]])
            return

        ctx.code.append((LOAD, "f" + s.name))
        ctx.stack(1)
        for y in x[1:]:
            f_Lcompile(y)
        ctx.code.append((CALL_FUNCTION, len(x)-1))
        ctx.stack(-(len(x) - 1))
        return

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

def f_Lpython(x):
    return eval(x)

def f_Leval(x):
    global ctx
    octx = ctx
    ctx = Context()
    f_Lcompile(x)
    code = ctx.make_code("eval", False)
    ctx = octx
    return eval(code)

def f_Lstr(x):
    if isinstance(x, list):
        return "(" + " ".join(map(f_Lstr, x)) + ")"
    else:
        return str(x)

def f_Leval_2bprint(x):
    f_Lout("--> " + f_Lstr(f_Leval(x)) + "\n")

def f_Lloopf(cond, body):
    while cond():
        body()

def f_Lmapn(f, count):
    for i in range(count):
        f(i)

def f_Lmapl(f, L):
    for i in L:
        f(i)

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
