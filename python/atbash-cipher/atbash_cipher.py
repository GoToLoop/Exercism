ENC = str.maketrans(ABC := 'abcdefghijklmnopqrstuvwxyz', ABC[::-1])

def encode(plain: str):
    print(plain, plain := ''.join(c for c in plain.lower() if c.isalnum()))
    print(plain := ' '.join(plain[i:i+5] for i in range(0, len(plain), 5)))
    return print(plain := plain.translate(ENC)) or plain


def decode(coded: str): return coded.replace(' ', '').translate(ENC)
