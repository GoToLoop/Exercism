package chance; import "math/rand"

// RollADie returns a random int d with 1 <= d <= 20.
func RollADie() int { return 1 + rand.Intn(20) }

// GenerateWandEnergy returns a random float64 f with 0.0 <= f < 12.0.
func GenerateWandEnergy() float64 { return 12 * rand.Float64() }

// ShuffleAnimals returns a slice with all eight animal strings in random order.
func ShuffleAnimals() (perms []string) { perms = make([]string, 8)
	for i, rnd := range rand.Perm(8) { perms[i] = animals[rnd] }; return }

var animals = [...]string {
	"ant", "beaver", "cat", "dog", "elephant", "fox", "giraffe", "hedgehog" }
