# ============================================
# Pure Lisp with REPL and standard library
# ============================================

def debug(*args,**kwargs):
    if False:
        print(*args,**kwargs)

import re

NIL = []
SYM_T = "T"

# --------------------------------------------
# Reader
# --------------------------------------------

def tokenize(src):
    # Remove everything from ';' to end of line
    src_no_comments = re.sub(r";[^\n]*", "", src)
    spaced = re.sub(r"('|`|,@|,|\(|\))", r" \1 ", src_no_comments)
    return spaced.split()

def read_from_tokens(tokens):
    if not tokens:
        raise SyntaxError("Unexpected EOF")
    token = tokens.pop(0)
    if token == "'":
        return ['quote', read_from_tokens(tokens)]
    if token == '`':
        return ['quasiquote', read_from_tokens(tokens)]
    if token == ',':
        return ['unquote', read_from_tokens(tokens)]
    if token == ',@':
        return ['unquote-splicing', read_from_tokens(tokens)]

    if token == '(':
        lst = []
        while tokens[0] != ')':
            lst.append(read_from_tokens(tokens))
        tokens.pop(0)
        return lst
    elif token == ')':
        raise SyntaxError("Unexpected )")
    else:
        return atom(token)

def atom(token):
    # NIL and T stay symbolic
    if token == "NIL":
        return NIL
    if token == "T":
        return SYM_T

    # Try float
    try:
        return float(token)
    except ValueError:
        pass

    # Otherwise it's a symbol
    return token

def read(src):
    tokens = tokenize(src)
    expr = read_from_tokens(tokens)
    if tokens:
        raise SyntaxError("Extra tokens after expression")
    return expr

# --------------------------------------------
# File loader
# --------------------------------------------
def lisp_load(filename, env):
    with open(filename, "r") as f:
        src = f.read()
    # Allow multiple top-level forms
    tokens = tokenize(src)
    while tokens:
        expr = read_from_tokens(tokens)
        lisp_eval(expr, env)


# --------------------------------------------
# Quasiquote expander
# --------------------------------------------
def qq_expand(expr):
    # Atom > literal
    if is_atom(expr):
        return ['quote', expr]

    # (unquote X)
    if isinstance(expr, list) and expr and expr[0] == 'unquote':
        return expr[1]

    # (unquote-splicing X) is illegal at top level
    if isinstance(expr, list) and expr and expr[0] == 'unquote-splicing':
        raise SyntaxError("unquote-splicing not in list context")

    # Otherwise it's a list - walk it
    result = NIL
    # Build list backwards
    for elem in reversed(expr):
        if isinstance(elem, list) and elem and elem[0] == 'unquote-splicing':
            # (append elem result)
            result = ['append', elem[1], result]
        else:
            # (cons expanded-elem result)
            result = ['cons', qq_expand(elem), result]

    return result


# --------------------------------------------
# Environment
# --------------------------------------------

def env_lookup(sym, env):
    for (k, v) in env:
        if k == sym:
            return (v, True)
    # raise NameError(f"Unbound symbol: {sym}")
    return (None, False)

def env_extend(params, args, env):
    if len(params) != len(args):
        raise TypeError("Arity mismatch")
    return list(zip(params, args)) + env


# --------------------------------------------
# Predicates
# --------------------------------------------

def is_atom(x):
    if x == NIL:
        return True
    return not isinstance(x, list)

def is_eq(x, y):
    return x == y


# --------------------------------------------
# EVAL
# --------------------------------------------

def lisp_eval(expr, env):
    if is_atom(expr):
        if expr is NIL:
            return NIL
        if expr == SYM_T:
            return SYM_T
        if isinstance(expr, float):
            return expr
        ret, found = env_lookup(expr, env)
        if found:
            return ret
        elif isinstance(expr, str):
            # possible primitive function
            return expr

    if not expr:
        return NIL

    op = expr[0]
    args = expr[1:]

    # quote
    if op == "quote":
        return args[0]

    # quasiquote
    if op == "quasiquote":
        expanded = qq_expand(args[0])
        return lisp_eval(expanded, env)
        
    # lambda
    if op == "lambda":
        params, body = args
        return ("LAMBDA", params, body, env)

    # label (for recursion)
    if op == "label":
        name, lam = args
        fn = lisp_eval(lam, env)
        return ("LABEL", name, fn)

    # defmacro
    if op == "defmacro":
        name, lambda_expr = args
        # lambda_expr is (lambda (params) body)
        lam = lisp_eval(lambda_expr, env)
        # Convert lambda closure into a macro object
        if lam[0] != "LAMBDA":
            raise SyntaxError("defmacro requires a lambda")
        _, params, body, closure_env = lam
        macro_obj = ("MACRO", params, body, closure_env)
        env.insert(0, (name, macro_obj))
        return name

    # define (syntactic sugar)
    if op == "define":
        name, value_expr = args
        value = lisp_eval(value_expr, env)
        env.insert(0, (name, value))
        return name

    # cond (special form)
    if op == "cond":
        for clause in args:
            test, expr2 = clause
            if lisp_eval(test, env) != NIL:
                return lisp_eval(expr2, env)
        return NIL

    # Regular application: (f arg1 arg2 ...)
    fn_val = lisp_eval(op, env)
    
    # For macros, pass raw args; for functions, pass evaluated args
    if isinstance(fn_val, tuple) and fn_val[0] == "MACRO":
        # raw unevaluated arguments
        return lisp_apply(fn_val, args, env)
    else:
        # normal function application
        arg_vals = [lisp_eval(a, env) for a in args]
        return lisp_apply(fn_val, arg_vals, env)


# --------------------------------------------
# APPLY
# --------------------------------------------

def lisp_apply(fn, arg_vals, env):
    if isinstance(fn, str):
        return apply_primitive(fn, arg_vals, env)

    # Macro application
    if isinstance(fn, tuple) and fn[0] == "MACRO":
        _, params, body, closure_env = fn
        # Macro receives *raw* (unevaluated) argument expressions
        new_env = env_extend(params, arg_vals, closure_env)
        expanded = lisp_eval(body, new_env)
        # Now evaluate the expanded code in the *current* environment
        return lisp_eval(expanded, env)

    if isinstance(fn, tuple) and fn[0] == "LAMBDA":
        _, params, body, closure_env = fn
        new_env = env_extend(params, arg_vals, closure_env)
        return lisp_eval(body, new_env)

    if isinstance(fn, tuple) and fn[0] == "LABEL":
        _, name, inner_fn = fn
        labeled_env = [(name, fn)] + env
        return lisp_apply(inner_fn, arg_vals, labeled_env)

    raise TypeError(f"Cannot apply non-function: {fn}")


# --------------------------------------------
# Primitive functions
# --------------------------------------------

def apply_primitive(name, args, env):
    if name == "atom":
        return SYM_T if is_atom(args[0]) else NIL

    if name == "eq":
        return SYM_T if is_eq(args[0], args[1]) else NIL

    if name == "car":
        x = args[0]
        return NIL if not isinstance(x, list) or not x else x[0]

    if name == "cdr":
        x = args[0]
        return NIL if not isinstance(x, list) or not x else x[1:]

    if name == "cons":
        x, y = args
        if y is NIL:
            return [x]
        if not isinstance(y, list):
            raise TypeError("cons second arg must be list or NIL")
        return [x] + y

    if name == "+":
        return sum(args)
    
    if name == "-":
        if len(args) == 1:
            return -args[0]
        out = args[0]
        for a in args[1:]:
            out -= a
        return out
    
    if name == "*":
        out = 1
        for a in args:
            out *= a
        return out

    if name == "/":
        out = args[0]
        for a in args[1:]:
            out /= a
        return out

    if name == "//":
        out = args[0]
        for a in args[1:]:
            out //= a   # integer division
        return out

    if name == "%":
        out = args[0]
        for a in args[1:]:
            out %= a
        return out

    if name == "<":
        return SYM_T if args[0] < args[1] else NIL
    
    if name == ">":
        return SYM_T if args[0] > args[1] else NIL
    
    if name == "<=":
        return SYM_T if args[0] <= args[1] else NIL
    
    if name == ">=":
        return SYM_T if args[0] >= args[1] else NIL

    if name == "load":
        fname = args[0]
        if not isinstance(fname, str):
            raise TypeError("load expects a filename string")
        lisp_load(fname, env)
        return fname

    raise NameError(f"Unknown primitive: {name}")


# --------------------------------------------
# REPL
# --------------------------------------------

def repl():
    env = []
    # Load standard library
    for form in STANDARD_LIBRARY:
        debug(f'form={form}')
        lisp_eval(read(form), env)

    print("Pure Lisp REPL - Ctrl+C to exit")
    while True:
        try:
            src = input("> ").strip()
            if not src:
                continue
            expr = read(src)
            result = lisp_eval(expr, env)
            print("?", result)
        except KeyboardInterrupt:
            print("\nGoodbye.")
            break
        except Exception as e:
            print("Error:", e)


# --------------------------------------------
# Run REPL if executed directly
# --------------------------------------------

if __name__ == "__main__":
    repl()
