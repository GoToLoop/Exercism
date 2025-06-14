package interest; import "math"; const MAX = math.MaxInt32

// rates defines a 2d array of interest rates based on balance thresholds.
//
// Each inner array represents a pair: {balance threshold, interest rate}.
//
// The balance thresholds act as exclusive upper bounds, meaning:
// 	- If `balance < threshold`, the associated interest rate applies.
// 	- The last threshold (MAX) ensures all balances of 5000 or more are covered.
//
// Example interpretation for each index of the outer array:
// 	0: Balances below 0.00 (negative balances) get a rate of 3.213.
// 	1: Balances from 0.00 to 999.99 get a rate of 0.5.
// 	2: Balances from 1000.00 to 4999.99 get a rate of 1.621.
// 	3: Balances from 5000.00 and beyond get a rate of 2.475.
var rates = [...][2]float32{{0, 3.213}, {1000, .5}, {5000, 1.621}, {MAX, 2.475}}

// InterestRate returns the interest rate for the provided balance.
func InterestRate(balance float64) (rate float32) { for _, r := range rates {
	if balance < float64(r[0]) { rate = r[1]; break } }; return }

// Interest calculates the interest for the provided balance.
func Interest(b float64) float64 { return b / 100 * float64(InterestRate(b)) }

// AnnualBalanceUpdate calculates the annual balance update, taking into account
// the interest rate.
func AnnualBalanceUpdate(b float64) float64 { return b + Interest(b) }

// YearsBeforeDesiredBalance calculates the minimum number of years required
// to reach the desired balance.
func YearsBeforeDesiredBalance(b, t float64) int { if b >= t { return 0 }
	return 1 + YearsBeforeDesiredBalance(AnnualBalanceUpdate(b), t) }