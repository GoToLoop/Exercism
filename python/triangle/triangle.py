from collections.abc import Sequence, Callable

def valid_triangle(triangle_type_checker: Callable[[Sequence[float]], bool]):
    def inner_checker(sides: Sequence[float]):
        a, b, c = (sides := sorted(map( abs, sides[:3] )))
        return a * b * c != 0 and a + b >= c and triangle_type_checker(sides)

    return inner_checker

def equilateral(sides: Sequence[float]):
    return len( uniques := set(sides[:3]) ) == 1 and all(uniques)

@valid_triangle
def isosceles(sides: Sequence[float]): return 0 < len( set(sides) ) < 3

@valid_triangle
def scalene(sides: Sequence[float]): return len( set(sides) ) == 3
