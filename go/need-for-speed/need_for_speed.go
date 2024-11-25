package speed; type Track struct { distance int }
type Car struct { battery, batteryDrain, speed, distance int }

// NewCar creates a new remote controlled car with full battery and given
// specifications, such as speed in meters and battery drain percentage.
func NewCar(spd, drain int) Car { return Car{ 100, drain, spd, 0 } }

// NewTrack creates a new track.
func NewTrack(distance int) Track { return Track{ distance } }

// Drive drives the car one time. If there is not enough battery to drive
// one more time, the car will not move.
func Drive(car Car) Car { bat := car.battery - car.batteryDrain
	if bat >= 0 { car.battery = bat; car.distance += car.speed }; return car }

// CanFinish checks if a car is able to finish a certain track.
func CanFinish(c Car, t Track) bool {
	// Check if there's enough available battery charge for the difference between
	// track distance and covered distance, multiplied by battery drain per move,
	// then divided by car movement speed:
	return c.battery >= (t.distance - c.distance) * c.batteryDrain / c.speed }
