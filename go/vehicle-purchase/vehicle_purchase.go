package purchase

// NeedsLicense determines whether a license is needed to drive a type of
// vehicle. Only "car" and "truck" require a license.
func NeedsLicense(kind string) bool { return kind == "car" || kind == "truck" }

// ChooseVehicle recommends a vehicle for selection. It always recommends the
// vehicle that comes first in lexicographical order.
func ChooseVehicle(a, b string) string {
	if a > b { a = b }; return a + " is clearly the better choice." }

// CalculateResellPrice calculates the vehicle's resell cost based on its age.
func CalculateResellPrice(price, age float64) float64 { switch {
	case age >= 10: return price * .5; case age >= 3: return price * .7;
	default: return price * .8 } }
