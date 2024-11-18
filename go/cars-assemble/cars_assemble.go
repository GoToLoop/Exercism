package cars

// CalculateWorkingCarsPerHour calculates how many working cars are
// produced by the assembly line every hour.
func CalculateWorkingCarsPerHour(carsPerHour int, rate float64) float64 {
	return float64(carsPerHour) * rate / 100 }

// CalculateWorkingCarsPerMinute calculates how many working cars are
// produced by the assembly line every minute.
func CalculateWorkingCarsPerMinute(carsPerMinute int, rate float64) int {
	return int(CalculateWorkingCarsPerHour(carsPerMinute, rate) / 60) }

// CalculateCost works out the cost of producing the given number of cars.
func CalculateCost(cars int) uint { made := uint(cars)
	tens, solos := made / 10, made % 10; return 95_000 * tens + 10_000 * solos }
