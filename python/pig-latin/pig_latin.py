################################# Main Section #################################
from typing import Callable, Any
import re

VOWEL_PATTERN = re.compile('''
    [aeiou]+ # one or more single vowels
    | yt     # 'yt' as a vowel
    | xr     # 'xr' as a vowel
''', re.VERBOSE) # matches a vowel sequence

SUFFIX = 'ay' # all words end w/ 'ay'

def vowel_match(word: str): return VOWEL_PATTERN.search(word)

def translate(text: str): # check & apply a rule for each word in string
    for i,word in enumerate(words := text.split()): # iterate over each word
        for j in rules_indices: # iterate over each rule function pair
            if rule_checkers[j](word): # check for a regex rule match
                words[i] = rule_appliers[j](word) # apply rule and save it back
                break # only 1 rule should be applied

    return ' '.join(words) # convert array of words back to 1 string
#################################### Rule 4 ####################################
def check_rule_4(word: str): # 'y' after consonant(s) is treated as a vowel
    if not (m := vowel_match(word)): return word[-1] == 'y' # no vowels case
    return (idx := m.start()) and word[idx] == 'y' # there are vowels after 'y'


def apply_rule_4(word: str): # move all starting consonants after 'y'
    idx = (m := vowel_match(word)) and m.start() # index of 1st vowel after 'y'
    return (word[idx:] + word[:idx] if idx else word[-1] + word[:-1]) + SUFFIX
#################################### Rule 3 ####################################
def check_rule_3(word: str): # 'qu' preceded or not by a consonant
    if not (m := vowel_match(word)): return False # no vowels match found
    return word[idx := m.start()] == 'u' and word[idx-1] == 'q'


def apply_rule_3(word: str): # move all consonants plus 'u' to end of word
    idx = vowel_match(word).start() + 1 # index after vowel 'u'
    return word[idx:] + word[:idx] + SUFFIX
#################################### Rule 2 ####################################
def check_rule_2(word: str): return (m := vowel_match(word)) and m.start() > 0

def apply_rule_2(word: str): # move all starting consonants after vowel cluster
    idx = vowel_match(word).start() # 1st vowel index
    return word[idx:] + word[:idx] + SUFFIX
#################################### Rule 1 ####################################
def check_rule_1(word: str): return (m := vowel_match(word)) and not m.start()

def apply_rule_1(word: str): return word + SUFFIX # starting vowel sound case
################################## Find Rules ##################################
def find_rule_functions(start_name: str) -> tuple[Callable[[str], Any], ...]:
    return *(v for k,v in all_vars if k.startswith(start_name) and callable(v)),


all_vars = locals().items() # grab all global variables

rule_checkers: tuple[Callable[[str], bool], ...] = find_rule_functions('check')
rule_appliers: tuple[Callable[[str], str],  ...] = find_rule_functions('apply')

rules_indices = range(len(rule_checkers)) # shared indices for rule funct. pairs
################################################################################
