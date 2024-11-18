// Package weather provides functionality for storing weather forecasting.
package weather

// CurrentLocation stores the current location updated via Forecast() function.
var CurrentLocation string

// CurrentCondition stores the current weather condition updated by Forecast().
var CurrentCondition string

// Forecast function updates the current location and weather condition,
// and returns a string containing the location and current weather condition.
func Forecast(city, condition string) string {
	CurrentLocation, CurrentCondition = city, condition
	return CurrentLocation + " - current weather condition: " + CurrentCondition }
