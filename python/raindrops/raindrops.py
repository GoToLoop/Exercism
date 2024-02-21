NOISES = (3, 'Pling'), (5, 'Plang'), (7, 'Plong')

def convert(n: int):
    return ''.join(noise for prime, noise in NOISES if not n % prime) or str(n)
