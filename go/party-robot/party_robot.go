package partyrobot; import . "fmt"

// Welcome greets a person by name.
func Welcome(name string) string { return "Welcome to my party, " + name + "!" }

// HappyBirthday wishes happy birthday to a person and exclaims their age.
func HappyBirthday(name string, age int) string {
	return Sprintf("Happy birthday %s! You are now %d years old!", name, age) }

// AssignTable assigns a table to each guest.
func AssignTable(name string, table int, mate, dir string, dis float64) string {
	const ROBOT = "\nYou have been assigned to table %03d. Your table is %s, " +
		"exactly %.1f meters from here.\nYou will be sitting next to %s."

	return Welcome(name) + Sprintf(ROBOT, table, dir, dis, mate) }
