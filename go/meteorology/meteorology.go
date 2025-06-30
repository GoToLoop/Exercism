package meteorology; import "fmt"; type (TemperatureUnit uint8; SpeedUnit uint8)

const (Celsius TemperatureUnit = 0; Fahrenheit TemperatureUnit = 1)
func (t TemperatureUnit) String() string {if t == 0 {return "°C"}; return "°F"}

type Temperature struct { deg int8; t TemperatureUnit }
func (t Temperature) String() string {return fmt.Sprintf("%d %s", t.deg, t.t)}

const (KmPerHour SpeedUnit = 0;	MilesPerHour SpeedUnit = 1)
func (s SpeedUnit) String() string {if s == 0 {return "km/h"}; return "mph"}

type Speed struct {	mag uint8; s SpeedUnit }
func (s Speed) String() string {return fmt.Sprintf("%d %s", s.mag, s.s)}

type MeteorologyData struct { loc string; t Temperature
	wdir string; wspd Speed; hum uint8 }
func (m MeteorologyData) String() string {return fmt.Sprintf(
	"%s: %v, Wind %s at %v, %d%% Humidity", m.loc, m.t,	m.wdir, m.wspd, m.hum)}
