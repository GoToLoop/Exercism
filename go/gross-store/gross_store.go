package gross

// units map represents a gross unit type and its corresponding amount.
var units = map[string]int{ "quarter_of_a_dozen": 3, "half_of_a_dozen": 6,
	"dozen": 12, "small_gross": 120, "gross": 144, "great_gross": 1728 }

// Units stores the Gross Store unit measurements.
func Units() map[string]int { return units }

// NewBill creates a new bill.
func NewBill() map[string]int { return map[string]int{} }

// AddItem adds an item to customer bill.
func AddItem(bill, units map[string]int, item, unit string) bool {
	amount := units[unit]; if amount == 0 { return false } // invalid unit
	bill[item] += amount; return true } // add or update item by unit amount

// RemoveItem removes an item from customer bill.
func RemoveItem(bill, units map[string]int, item, unit string) bool {
	b, u := bill[item], units[unit]; qty := b - u // new calculated amount
	if b * u == 0 || qty < 0 { return false } // inexisting item or unit
	if qty != 0 { bill[item] = qty } else { delete(bill, item) }; return true }

// GetItem returns the quantity of an item that the customer has in their bill.
func GetItem(bill map[string]int, item string) (int, bool) {
	qty, exists := bill[item]; return qty, exists }
