"""
Functions to help edit essay homework using string manipulation.
"""

capitalize_title = lambda sentence='': sentence.title()
"""Converts the 1st letter of each word in the sentence to uppercase.

:param sentence: str - words that needs title casing.
:return: str - words in title case (first letters capitalized).
"""

check_sentence_ending = lambda sentence='': sentence.endswith('.')
"""Checks the ending of the sentence to verify that a period is present.

:param sentence: str - a sentence to check.
:return: bool - return True if punctuated correctly with period.
"""

clean_up_spacing = lambda sentence='': sentence.strip()
"""Removes any whitespace at the start and end of the sentence.

:param sentence: str - a sentence to clean of leading and trailing spaces.
:return: str - a sentence cleaned of leading and trailing space characters.
"""

replace_word_choice = lambda txt='', old='', new='': txt.replace(old, new)
"""Replaces all occurrencies of a word in the provided text with a new one.

:param txt: str - a sentence to replace words in.
:param old: str - word to replace.
:param new: str - replacement word.
:return: str - input sentence with a new word in place of an old word.
"""
