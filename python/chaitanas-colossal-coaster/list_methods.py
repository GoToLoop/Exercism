"""Functions to manage and organize queues at Chaitana's roller coaster."""

def add_me_to_the_queue(fast: list[str], slow: list[str], type: int, name: str):
    """Adds a person to the 'express' or 'normal' queue based on ticket number.

    :param fast: list[str] - names in the Fast-track express queue.
    :param slow: list[str] - names in the normal queue.
    :param type: int - type of ticket. 1 = express, 0 = normal.
    :param name: str - name of person to add to a queue.

    :return: list[str] - the (updated) queue the name was added to.
    """

    return fast.append(name) or fast if type else slow.append(name) or slow


find_my_friend = lambda queue=[''], friend='': queue.index(friend)
"""Searches the queue for a name and return their queue position (index).

:param queue: list[str] - names in the queue.
:param friend: str - name of friend to find.

:return: int - index at which the friends name was found.
"""


add_me_with_my_friends = lambda q=[''], i=0, s='': q.insert(i, s) or q
"""Inserts the late arrival's name at a specific index of the queue.

:param q: list[str] - names in the queue.
:param i: int - the index at which to add the new name.
:param s: str - the name to add.

:return: list[str] - queue updated with new name.
"""


remove_the_mean_person = lambda queue=[''], bad='': queue.remove(bad) or queue
"""Removes the mean person from the queue by the provided name.

:param queue: list[str] - names in the queue.
:param bad: str - name of the mean person.

:return: list[str] - queue update with the mean person's name removed.
"""


how_many_namefellows = lambda queue=[''], fellow='': queue.count(fellow)
"""Counts how many times the provided name appears in the queue.

:param queue: list[str] - names in the queue.
:param fellow: str - name you wish to count or track.

:return: int - the number of times the name appears in the queue.
"""


remove_the_last_person = lambda queue=['']: queue.pop()
"""Removes person in last index (tail) from the queue and return their name.

:param queue: list[str] - names in the queue.
:return: str - name that has been removed from the end of the queue.
"""


sorted_names = lambda queue=['']: sorted(queue)
"""Sort the names in the queue in alphabetical order and return the result.

:param queue: list[str] - names in the queue.
:return: list[str] - copy of the queue in alphabetical order.
"""
