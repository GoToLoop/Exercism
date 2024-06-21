"""Functions to help play and score a game of Blackjack.

How to play blackjack:    https://bicyclecards.com/how-to-play/blackjack/
"Standard" playing cards: https://en.wikipedia.org/wiki/Standard_52-card_deck
"""

DECK = {**{str(i): i for i in range(2, 11)}, **{'A':1, 'J':10, 'Q':10, 'K':10}}

value_of_card = lambda card='A': DECK[card.strip().upper()]
"""Determine the scoring value of a card.

:param card: str - given card.
:return: int - value of a given card.  See below for values.

1. 'J', 'Q', or 'K' (otherwise known as "face cards") = 10
2. 'A' (ace card) = 1
3. '2' - '10' = numerical value
"""


def higher_card(a: str, b: str):
    """Determine which card has a higher value in the hand.

    :params a, b: str - cards dealt in hand. See below for values.
    :return: str or tuple[str, str] - resulting Tuple contains both cards if
    they are of equal value.

    1. 'J', 'Q', or 'K' (otherwise known as "face cards") = 10
    2. 'A' (ace card) = 1
    3. '2' - '10' = numerical value
    """

    return (a, b) if (x := DECK[a]) == (y := DECK[b]) else x > y and a or b


def value_of_ace(a: str, b: str):
    """Calculate the most advantageous value for the ace card.

    :params a, b: str - cards dealt. See below for values.
    :return: Literal[1, 11] - either 1 or 11 value of the upcoming ace card.

    1. 'J', 'Q', or 'K' (otherwise known as "face cards") = 10
    2. 'A' (ace card) = 11 (if already in hand)
    3. '2' - '10' = numerical value
    """

    return int(1 in (x := DECK[a], y := DECK[b])) or (x + y + 11 < 22) * 10 + 1


is_blackjack = lambda a='A', b='K', JACK={ 1, 10 }: JACK == { DECK[a], DECK[b] }
"""Determine if the hand is a 'natural' or 'blackjack'.

:params a, b: str - cards dealt. See below for values.
:return: bool - is the hand is a blackjack (two cards worth 21).

1. 'J', 'Q', or 'K' (otherwise known as "face cards") = 10
2. 'A' (ace card) = 11 (if already in hand)
3. '2' - '10' = numerical value
"""


can_split_pairs = lambda a='J', b='K': DECK[a] == DECK[b]
"""Determine if a player can split their hand into two hands.

:params a, b: str - cards dealt.
:return: bool - can the hand be split into two pairs? (i.e. cards are of
the same value).
"""


can_double_down = lambda a='A', b='9': 9 <= DECK[a] + DECK[b] <= 11
"""Determine if a blackjack player can place a double down bet.

:params a, b: str - first and second cards in hand.
:return: bool - can the hand can be doubled down? (i.e. totals 9, 10 or
11 points).
"""
