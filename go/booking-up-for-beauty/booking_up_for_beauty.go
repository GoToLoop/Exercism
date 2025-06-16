package booking; import t "time"; const L1 = "1/2/2006 15:04:05"
const L2 = "January 2, 2006 15:04:05"; const L3 = "Monday, " + L2
const L4 = "You have an appointment on Monday, January 2, 2006, at 15:04."

// Schedule returns a time.Time from a string containing a date.
func Schedule(date string) t.Time { s,_ := t.Parse(L1, date); return s }

// HasPassed returns whether a date has passed.
func HasPassed(d string) bool {	s,_ := t.Parse(L2, d); return t.Now().After(s) }

// IsAfternoonAppointment returns whether a time is in the afternoon.
func IsAfternoonAppointment(date string) bool {
	s,_ := t.Parse(L3, date); h := s.Hour(); return 12 <= h && h < 18 }

// Description returns a formatted string of the appointment time.
func Description(date string) string { return Schedule(date).Format(L4) }

// AnniversaryDate returns a Time with this year's anniversary.
func AnniversaryDate() t.Time {
	return t.Date(t.Now().Year(), t.September, 15, 0, 0, 0, 0, t.UTC) }
