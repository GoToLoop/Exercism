"""Functions to help Azara and Rui locate pirate treasure.
"""

get_coordinate = lambda record=('', ''): record[1]
"""Returns coordinate value from a tuple containing the treasure name, and
treasure coordinate.

:param record: tuple[str, str] - with a (treasure, coordinate) pair.

:return: str - the extracted map coordinate.
"""

convert_coordinate = tuple
"""Splits the given coordinate into a tuple containing its individual
components.

:param coordinate: str - a string map coordinate.

:return: tuple[str, str] - the string coordinate split into its individual
components.
"""

compare_records = lambda a, b: convert_coordinate(get_coordinate(a)) == b[1]
"""Compares two record types and determine if their coordinates match.

:param a: tuple[str, str] - Azara's (treasure, coordinate) pair.
:param b: tuple[str, tuple[str, str], str] - Rui's\
(location, (coordinates), quadrant) trio.

:return: bool - do the coordinates match?
"""

create_record = lambda a, b: compare_records(a, b) and a + b or "not a match"
"""Combines and returns the two record types (if possible).

:param a: tuple[str, str] - Azara's (treasure, coordinate) pair.
:param b: tuple[str, tuple[str, str], str] - Rui's\
(location, (coordinates), quadrant) trio.

:return: tuple[str, str, str, tuple[str, str], str] |
Literal['not a match'] - the combined record (if compatible), or a warn
message (if incompatible).
"""

def clean_up(combo: tuple[tuple[str, str, str, tuple[str, str], str], ...]):
    """Cleans up a combined record group into a multi-line string of
    single records.

    :param combo: tuple[tuple[str, str, str, tuple[str, str], str], ...] -\
    combined records from both participants.

    :return: str - a "cleaned" multi-line string with items separated by
    newlines, with excess 1st coordinates removed.
    """

    return print(s := ''.join(str(r[:1] + r[2:]) + '\n' for r in combo)) or s
