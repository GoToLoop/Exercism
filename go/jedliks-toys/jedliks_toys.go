package jedlik; import f "fmt"

// DRIVEN and BATTERY are format strings used for displaying the car's
// total distance driven and remaining battery percentage, respectively.
// The `%d` verb is replaced with an integer value (interpolation) at runtime.
const DRIVEN, BATTERY = "Driven %d meters", "Battery at %d%%"

// Drive advances the car by its speed if there is enough battery for a full
// move. Each call reduces the battery by batteryDrain units. If battery is
// insufficient, no changes are made to the car's fields.
func (c *Car) Drive() { if bat := c.battery - c.batteryDrain; bat >= 0 {
	c.battery = bat; c.distance += c.speed } }

// DisplayDistance returns a string representing the total distance the car has
// driven, formatted for display.
func (c *Car) DisplayDistance() string { return f.Sprintf(DRIVEN, c.distance) }

// DisplayBattery returns a string representing the current battery percentage
// of the car, formatted for display.
func (c *Car) DisplayBattery() string { return f.Sprintf(BATTERY, c.battery) }

// CanFinish determines whether the car has enough battery to finish a track
// of the given distance. The car can only move if it has enough charge to
// perform a full push (i.e., battery won't drop below zero after a move).
func (c *Car) CanFinish(trackDistance int) bool {
	// `trackDistance / c.speed` represents how many car moves are required.
	// Then `c.batteryDrain` multiplication gives the minimum required battery.
	return c.battery >= c.batteryDrain * trackDistance / c.speed }
