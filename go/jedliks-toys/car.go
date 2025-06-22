package jedlik

// Car represents a remote-controlled car on a race track.
type Car struct {
	// speed is how far in meters the car covers per batteryDrain units.
	speed 			int

	// batteryDrain is the amount of battery consumed per car move.
	batteryDrain 	int

	// battery holds the current remaining battery percentage.
	battery 		int

	// distance tracks the current distance the car has traveled in meters.
	distance 		int
}

// NewCar returns a new Car with the specified speed and battery drain.
// The car starts with a full battery and zero distance traveled.
func NewCar(speed, batteryDrain int) *Car {
	return &Car{
		speed: 			speed,
		batteryDrain: 	batteryDrain,
		battery:      	100,
		distance: 		0,
	}
}
