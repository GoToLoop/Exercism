"""
    Functions for organizing and calculating student exam scores.
"""


round_scores = lambda student_scores=[0, .1]: [*map(round, student_scores)]
"""Round all provided student scores as a new list of ints.

:param student_scores: list[int | float] - float or int of student exam scores

:return: list[int] - student scores rounded to nearest integer value
"""


count_failed_students = lambda scores=[0]: sum(1 for s in scores if s <= 40)
"""Count the number of failing students out of the group provided.

:param scores: list[int] - containing integer student scores

:return: int - count of student scores which are at or below 40
"""


above_threshold = lambda scores=[0], best=0: [s for s in scores if s >= best]
"""Determine how many of the provided student scores were 'the best'
based on the provided threshold.

:param scores: list[int] - of integer scores
:param best: int - threshold to cross to be the "best" score

:return: list[int] - of integer scores that are at or above "best" threshold
"""


letter_grades = lambda highest=0: [*range(41, highest, highest - 40 >> 2)]
"""Create a list of grade thresholds based on the provided highest grade.

:param highest: int - value of highest exam score

:return: list[int] - of lower threshold scores for each D-A letter grade range

Example:
    For a highest score = 100, and failing is <= 40,
    the result intervals would be [41, 56, 71, 86]:

    41 <= "D" <= 55;
    56 <= "C" <= 70;
    71 <= "B" <= 85;
    86 <= "A" <= 100
"""


def student_ranking(scores: list[int], names: list[str], INFO='%d. %s: %d'):
    """Organize the student's rank, name, and grade information in
    descending order.

    :param scores: list[int] - of student scores in descending order
    :param names: list[str] - of names by exam score in descending order

    :return: list[str] - of strings in format ["<rank>. <name>: <score>"]
    """

    return [INFO % (i + 1, name, scores[i]) for i, name in enumerate(names)]


def perfect_score(students: list[list[str | int]]):
    """Create a list that contains the name and grade of the first student
    to make a perfect score on the exam.

    :param students: list[list[str | int]] - of [<student name>, <score>]

    :return: list[str | int] - first `[<student name>, 100]` found or `[]`
    if no student score of 100 is found
    """

    return next((lst for lst in students if lst[1] == 100), [])
