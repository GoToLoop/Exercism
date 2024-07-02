SQUARED_SCORES = (1, 10), (25, 5), (100, 1), (float('inf'), 0) # 1, 5, 10

def score(x: float, y: float):
    print(f'{x = }, {y = }, dist_sq =', dist_sq := x*x + y*y)

    for rad_sq, point in SQUARED_SCORES:
        if dist_sq <= rad_sq: return point


# score = lambda x, y: 5 * ((d := x*x + y*y) <= 1) + 4 * (d <= 25) + (d <= 100)
