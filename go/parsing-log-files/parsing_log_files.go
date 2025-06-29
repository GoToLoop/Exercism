package parsinglogfiles; import (s "strings"; r "regexp")

var tags = [...]string{ "[TRC]", "[DBG]", "[INF]", "[WRN]", "[ERR]", "[FTL]" }

var delim,pass = r.MustCompile("<[~*=-]*>"), r.MustCompile(`(?i)".*password.*"`)
var eol,tag = r.MustCompile(`end-of-line\d+`), r.MustCompile(`User\s+(\w+ )`)

func IsValidLine(text string) bool { for i := 0; i < len(tags); i++ {
	if s.HasPrefix(text, tags[i]) { return true } }; return false }

func SplitLogLine(text string) []string { return delim.Split(text, -1) }

func CountQuotedPasswords(lines []string) (count int) {
	for _,log := range lines { if pass.MatchString(log) { count++ } }; return }

func RemoveEndOfLineText(t string) string { return eol.ReplaceAllString(t, "") }

func TagWithUserName(lines []string) []string { for i,log := range lines {
	if matches := tag.FindStringSubmatch(log); len(matches) > 1 {
		lines[i] = "[USR] " + matches[1] + log } }; return lines }
