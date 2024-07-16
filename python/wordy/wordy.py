OPS = { 'plus': int.__add__, 'minus': lambda a, b: a - b, # the 4 operations
        'multiplied': int.__mul__, 'divided': int.__floordiv__ }

SYN = 'syntax error'; UNK = 'unknown operation' # the 2 possible errors

def num(val: str): return val.lstrip('-').isdigit() # if val represents an int

def answer(question: str) -> int:
    if not question.startswith('What is'): raise ValueError(UNK)

    print( parts := question[8:-1].replace('by ', '').split() )

    if parts and num(parts[0]): total = int(parts.pop(0)) # pops head as int
    else: raise ValueError(SYN) # list's head isn't convertable to int!

    if len(parts) & 1: # fails if list has an odd size!
        raise ValueError( (num(p := parts[-1]) or p in OPS) and SYN or UNK )

    for i in range(0, len(parts), 2): # iterate as (str op, int val) pair 
        if (op := parts[i]) not in OPS: raise ValueError(num(op) and SYN or UNK)
        # if not num(val := parts[i + 1]): raise ValueError(SYN)

        total = OPS[op](total, int(parts[i + 1]))
    
    return total
