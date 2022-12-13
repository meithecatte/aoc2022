import ast

def cmp(a, b):
    if type(a) is int and type(b) is int:
        if a < b: return '<'
        elif a == b: return '='
        else: return '>'
    elif type(a) is list and type(b) is list:
        for aa, bb in zip(a, b):
            c = cmp(aa, bb)
            if c != '=': return c
        if len(a) < len(b): return '<'
        elif len(a) == len(b): return '='
        else: return '>'
    elif type(a) is list:
        return cmp(a, [b])
    else:
        return cmp([a], b)

class Compare(tuple):
    def __lt__(self, other):
        return cmp(list(self), list(other)) == '<'

def handle_pair(pair):
    a, b = pair.strip().split('\n')
    a = ast.literal_eval(a)
    b = ast.literal_eval(b)
    return cmp(a, b)

def testcase(fname):
    print(fname)
    with open(fname) as f:
        data = f.read()
    pairs = data.split('\n\n')
    results = [handle_pair(x) for x in pairs]
    s = 0
    for i, c in enumerate(results):
        if c == '<':
            s += i + 1
    print(s)

    sep1 = [[2]]
    sep2 = [[6]]
    inp = [ast.literal_eval(x) for x in data.split('\n') if x != '']
    inp += [sep1, sep2]
    inp.sort(key=Compare)
    A = inp.index(sep1)
    B = inp.index(sep2)
    print((A + 1) * (B + 1))

testcase('example.in')
testcase('puzzle.in')
