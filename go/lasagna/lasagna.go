package lasagna

// The expected oven time in minutes & minutes to prepare a single layer.
const OvenTime, minutesPerLayer = 40, 2

// RemainingOvenTime returns the remaining minutes based on the `actual`
// minutes already in the oven.
func RemainingOvenTime(elapsed int) int { return OvenTime - elapsed }

// PreparationTime calculates the time needed to prepare the lasagna based on
// the amount of layers.
func PreparationTime(layers int) int { return layers * minutesPerLayer }

// ElapsedTime calculates the time elapsed cooking the lasagna. This time
// includes the preparation time and the time the lasagna is baking in the oven.
func ElapsedTime(lays, time int) int { return PreparationTime(lays) + time }
