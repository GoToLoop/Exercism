! The tests were created from https://github.com/exercism/problem-specifications/blob/main/exercises/reverse-string/canonical-data.json

PROGRAM reverse_string_test_main
  USE TesterMain
  USE reverse_string
  IMPLICIT NONE

  ! Test 1: an empty string
  CALL assert_equal("", reverse(""), "an empty string")

  ! Test 2: a word
  CALL assert_equal("tobor", reverse("robot"), "a word")

  ! Test 3: a capitalized word
  CALL assert_equal("nemaR", reverse("Ramen"), "a capitalized word")

  ! Test 4: a sentence with punctuation
  CALL assert_equal("!yrgnuh m'I", reverse("I'm hungry!"), "a sentence with punctuation")

  ! Test 5: a palindrome
  CALL assert_equal("racecar", reverse("racecar"), "a palindrome")

  ! Test 6: an even-sized word
  CALL assert_equal("reward", reverse("drawer"), "an even-sized word")

  CALL test_report()

END PROGRAM

