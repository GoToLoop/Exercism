def find(lst: list[int], val: int):
    """Binary search to find the index of a value in a sorted list.

    :param list[int] lst: A sorted list of integers.
    :param int val: The value to search for.
    :raises ValueError: If the value is not found in the list.
    :return int: The index of the value in the list.
    """
    lo = 0; hi = len(lst) - 1; mid = hi >> 1

    while lo <= hi:
        if (v := lst[mid]) == val: return print(lst, val, mid) or mid
        (lo := mid + 1) if v < val else (hi := mid - 1); mid = lo + hi >> 1

    raise ValueError('value not in array')
