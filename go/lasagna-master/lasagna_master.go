package lasagna

func PreparationTime(layers []string, time int) int {
	if time <= 0 { time = 2 }; return len(layers) * time }

func Quantities(layers []string) (grams int, liters float64) {
	for _, layer := range layers { switch layer {
		case "noodles": grams += 50; case "sauce": liters += .2 } }; return }

func AddSecretIngredient(his, my []string) { my[len(my)-1] = his[len(his)-1] }

func ScaleRecipe(amounts []float64, portions int) (scaled []float64) {
	scale := .5 * float64(portions); scaled = append(scaled, amounts...)
	for idx := range scaled { scaled[idx] *= scale }; return }
