package annalyn

// CanFastAttack can be executed only when the knight is sleeping.
func CanFastAttack(knight bool) bool { return !knight }

// CanSpy can be executed if at least one of the characters is awake.
func CanSpy(k, a, p bool) bool { return k || a || p }

// CanSignalPrisoner can be executed if the prisoner is awake
// and the archer is sleeping.
func CanSignalPrisoner(a, p bool) bool { return p && !a }

// CanFreePrisoner can be executed if the prisoner is awake and the other
// 2 characters are asleep or if Annalyn's pet dog is with her
// and the archer is sleeping.
func CanFreePrisoner(knight, archer, prisoner, dog bool) bool {
	return CanSignalPrisoner(archer, prisoner) && !knight || dog && !archer }
