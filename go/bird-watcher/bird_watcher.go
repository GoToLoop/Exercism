package birdwatcher

// TotalBirdCount return the total bird count by summing
// the individual day's counts.
//
// Param:
//
// birds: a slice of integers where each integer represents
// the number of birds counted on a particular day in the garden.
//
// Returns:
//
// total: an integer representing the total count of birds since logs started.
func TotalBirdCount(birds []int) (total int) {
	for i, len := 0, len(birds); i < len; i++ { total += birds[i] }; return }

// BirdsInWeek returns the total bird count by summing
// only the items belonging to the given week.
//
// Params:
//
// birdsPerDay: a slice of integers where each integer represents
// the number of birds counted on a particular day.
//
// week: a 1-based index representing a chunk of 7 indices.
//
// Returns:
//
// An integer representing the total count of birds for the specified week.
func BirdsInWeek(birdsPerDay []int, week int) int {
	week *= 7; return TotalBirdCount( birdsPerDay[week - 7 : week] ) }

// FixBirdCountLog fixes the untracked hidden bird mistake by increasing the
// bird count by one in place for alternate days (even indices).
//
// Param:
//
// birds: a slice of integers representing the log of daily bird sightings
// in the garden.
//
// Returns:
//
// The corrected bird count slice, incremented for every even index.
func FixBirdCountLog(birds []int) []int {
	for i, len := 0, len(birds); i < len; i += 2 { birds[i]++ }; return birds }
