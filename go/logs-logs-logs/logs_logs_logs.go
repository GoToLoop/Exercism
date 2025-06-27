package logs; import (s "strings"; "unicode/utf8"); var apps = map[rune]string{
	'❗': "recommendation", '🔍': "search", '☀': "weather"}

// Application identifies the application emitting the given log.
func Application(log string) string {
	if i := s.IndexAny(log, "❗🔍☀"); i >= 0 { ch, _ :=
		utf8.DecodeRuneInString(log[i:]); return apps[ch] }; return "default" }

// Replace replaces all occurrences of old with new, returning the modified log
// to the caller.
func Replace(log string, oldRune, newRune rune) string {
	return s.ReplaceAll(log, string(oldRune), string(newRune)) }

// WithinLimit determines whether or not the number of characters in log is
// within the limit.
func WithinLimit(log string, limit int) bool {
	return utf8.RuneCountInString(log) <= limit }
