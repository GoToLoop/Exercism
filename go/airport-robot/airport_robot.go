package airportrobot; type Italian struct {}; type Portuguese struct {}
type Greeter interface { LanguageName() string; Greet(name string) string }

func SayHello(n string, greeter Greeter) string {
	return "I can speak " + greeter.LanguageName() + ": " + greeter.Greet(n) }

func (Italian) LanguageName() string { return "Italian" }
func (Italian) Greet(name string) string { return "Ciao " + name + "!" }

func (Portuguese) LanguageName() string { return "Portuguese" }
func (Portuguese) Greet(name string) string { return "Olá " + name + "!" }
