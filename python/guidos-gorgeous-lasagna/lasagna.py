"""Functions used in preparing Guido's gorgeous lasagna.

Learn about Guido, the creator of the Python language:
https://en.wikipedia.org/wiki/Guido_van_Rossum

This is a module docstring, used to describe the functionality
of a module and its functions and/or classes.
"""

EXPECTED_BAKE_TIME = 40
PREPARATION_TIME = 2

def bake_time_remaining(elapsed_bake_time: int, _expected_bake_time=EXPECTED_BAKE_TIME):
    """Calculate the bake time remaining.

    :param elapsed_bake_time: int - baking time already elapsed.
    :return: int - remaining bake time (in minutes) derived from 'EXPECTED_BAKE_TIME'.

    Function that takes the actual minutes the lasagna has been in the oven as
    an argument and returns how many minutes the lasagna still needs to bake
    based on the `EXPECTED_BAKE_TIME`. A negative minute means baking time is overdue!
    """

    return abs(_expected_bake_time) - abs(elapsed_bake_time)


def preparation_time_in_minutes(number_of_layers: int, _preparation_time=PREPARATION_TIME):
    """Calculate preparation time based on number of layers.

    :param number_of_layers: int - the number of layers in the lasagna.
    :return: int - number of minutes to prepare the layers derived from 'PREPARATION_TIME'.

    Function takes the number of lasagna layers and returns how many minutes are required
    to prepare them (in minutes) derived from 'PREPARATION_TIME'.
    """

    return abs(_preparation_time) * abs(number_of_layers)


def elapsed_time_in_minutes(number_of_layers: int, elapsed_bake_time: int,
                            _prep_t=PREPARATION_TIME):
    """Calculate the elapsed cooking time.

    :param number_of_layers: int - the number of layers in the lasagna.
    :param elapsed_bake_time: int - elapsed cooking time.
    :return: int - total time elapsed (in minutes) preparing and cooking.

    This function takes two integers representing the number of lasagna layers and the time
    already spent baking and calculates the total elapsed minutes spent cooking the lasagna.
    """

    return preparation_time_in_minutes(number_of_layers, _prep_t) + abs(elapsed_bake_time)
