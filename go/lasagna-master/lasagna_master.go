package lasagna

func PreparationTime(layers []string, time int) int {
	if time <= 0 { time = 2 }; return time * len(layers) }

func Quantities(layers []string) (grams int, liters float64) {
	for _, layer := range layers { switch layer {
		case "noodles": grams += 50; case "sauce": liters += .2 } }; return }

func AddSecretIngredient(his, my []string) { my[len(my)-1] = his[len(his)-1] }

func ScaleRecipe(amounts []float64, portions int) []float64 {
	scaled, scale := make([]float64, len(amounts)), .5 * float64(portions)
	for idx, qty := range amounts { scaled[idx] = qty * scale }; return scaled }
