package cards

// FavoriteCards returns a slice with the cards 2, 6 and 9 in that order.
func FavoriteCards() []int { return []int{ 2, 6, 9 } }

// GetItem retrieves an item from a slice at given position.
// If the index is out of range, we want it to return -1.
func GetItem(l []int, i int) int { if inRange(l, i) { return l[i] }; return -1 }

// SetItem writes an item to a slice at given position overwriting an
// existing value. If the index is out of range the value needs to be appended.
func SetItem(lst []int, i, v int) []int {
	if inRange(lst, i) { lst[i] = v } else { lst = append(lst, v) }; return lst }

// PrependItems adds an arbitrary number of values at the front of a slice.
func PrependItems(lst []int, vals ...int) []int { return append(vals, lst...) }

// RemoveItem removes an item from a slice by modifying the existing slice.
func RemoveItem(lst []int, i int) []int {
	if inRange(lst, i) { lst = append(lst[:i], lst[i+1:]...) }; return lst }

// inRange checks if the given index is within the bounds of the provided slice.
// It returns true if the index is valid, otherwise it returns false.
//
// Parameters:
// 	- lst: A slice of any type.
// 	- idx: The index to check.
//
// Returns:
// 	- true if the index is within bounds, false otherwise.
func inRange[T any](lst []T, idx int) bool { return idx >= 0 && idx < len(lst) }
