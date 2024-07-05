"""
Functions for tracking poker hands and assorted card tasks.
Python list docs: https://docs.Python.org/3/tutorial/datastructures.html
"""

get_rounds = lambda round=0: [ round, round + 1, round + 2 ]; Z = [0]
"""Creates a list containing the current and next two round numbers.

:param round: int - current round number.
:return: list[int] - current round and the two that follow.
"""

concatenate_rounds = lambda rounds_1=Z, rounds_2=Z: rounds_1 + rounds_2
"""Concatenates two lists consisting of the number of rounds played.

:param rounds_1: list[int] - first rounds played.
:param rounds_2: list[int] - second set of rounds played.
:return: list[int] - a new list with all rounds played from both lists.
"""

list_contains_round = lambda rounds=Z, round=0: round in rounds
"""Checks if the list of rounds contains the specified number.

:param rounds: list[int] - rounds played.
:param round: int - round number.
:return: bool - was the round played?
"""

card_average = lambda hand=Z: sum(hand) / len(hand)
"""Calculates and returns the average of the card values in the list.

:param hand: list[int] - cards in hand.
:return: float - average value of the cards in the hand.
"""

def approx_average_is_average(h: list[int]):
    """Returns if the (average of first and last card values) OR
    ('middle' card) == calculated true average.

    :param h: list[int] - cards in hand.
    :return: bool - does one of the average types equal the true average?
    """

    return (a := card_average(h)) == (h[0] + h[-1]) / 2 or a == h[len(h) // 2]
    # return card_average(h) in ((h[0] + h[-1]) / 2, h[len(h) // 2])


def average_even_is_average_odd(hand: list[int]):
    """Returns if the (average of even indexed card values) ==
    (average of odd indexed card values).

    :param hand: list[int] - cards in hand.
    :return: bool - are even and odd averages equal?
    """

    return card_average(hand[::2]) == card_average(hand[1::2])


maybe_double_last = lambda h=Z: h[:-1] + [h[-1] + 11 * (h[-1] == 11)]
"""Multiplies a Jack card value (11) in the last index position by 2.

:param h: list[int] - cards in hand.
:return: list[int] - new hand with last card Jack (if present) value doubled.
"""
