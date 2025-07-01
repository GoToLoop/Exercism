package sorting; import (p "fmt"; s "strconv");

type NumberBox interface{ Number() int }
type (FancyNumberBox interface{ Value() string }; FancyNumber struct{n string})

// Value is a FancyNumber struct method implementing FancyNumberBox interface,
// which returns its boxed number that's stored in string type.
func (fn FancyNumber) Value() string { return fn.n }

const NUM, BOX = "This is the number ", "This is a box containing the number "
const FAN = "This is a fancy box containing the number "

// DescribeNumber should return a string describing the number.
func DescribeNumber(f float64) string { return NUM + p.Sprintf("%.1f", f) }

// DescribeNumberBox should return a string describing the NumberBox.
func DescribeNumberBox(nb NumberBox) string {
	return BOX + s.Itoa(nb.Number()) + ".0" }

// ExtractFancyNumber should return the integer value for a FancyNumber
// and 0 if any other FancyNumberBox is supplied.
func ExtractFancyNumber(fnb FancyNumberBox) int {
	fn, _ := fnb.(FancyNumber); i, _ := s.Atoi(fn.n); return i }

// DescribeFancyNumberBox should return a string describing the FancyNumberBox.
func DescribeFancyNumberBox(fnb FancyNumberBox) string {
	return FAN + s.Itoa(ExtractFancyNumber(fnb)) + ".0" }

// DescribeAnything should return a string describing whatever it contains.
func DescribeAnything(i any) (msg string) { switch v := i.(type) {
	case int:				msg = DescribeNumber(float64(v))
	case float64:			msg = DescribeNumber(v)
	case NumberBox:			msg = DescribeNumberBox(v)
	case FancyNumberBox:	msg = DescribeFancyNumberBox(v)
	default:				msg = "Return to sender"}; return }
