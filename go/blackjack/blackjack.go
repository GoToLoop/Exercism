package blackjack

// A map of Blackjack card values, indicating each card's worth in the game.
// Face cards (jack, queen, king) are worth 10, and an ace can be worth 11.
var deck = map[string]int{ "two": 2, "three": 3, "four": 4, "five": 5,
	"six": 6, "seven": 7, "eight": 8, "nine": 9, "ten": 10,
	"jack": 10, "queen": 10, "king": 10, "ace": 11 }

// ParseCard returns the integer value of a card following Blackjack ruleset.
func ParseCard(card string) int { return deck[card] }

// FirstTurn returns the decision for the first turn, given two cards of the
// player and one card of the dealer. It decides whether the player should
// "P" (Split), "W" (Win), "S" (Stand), or "H" (Hit).
func FirstTurn(card1, card2, dealer string) string {
	switch sum, val := ParseCard(card1) + ParseCard(card2), ParseCard(dealer); {

	// Split if the sum of the two cards is 22 (i.e., two aces):
	case sum == 22: return "P" // Split

	// Win if the dealer's card is less than 10. Stand otherwise:
	case sum == 21: if val < 10 { return "W" }; return "S" // Win or Stand

	// Stand if the sum of the player's cards is in range 17 to 20:
	case sum >= 17: return "S" // Stand

	// For the case the sum of the player's hand is within range 12 to 16.
	// Stand if the dealer's card is less than 7. Hit otherwise:
	case sum >= 12: if val < 7 { return "S" }; return "H" // Stand or Hit

	// Hit if the sum of the player's cards is less than 12:
	default: return "H" } } // Hit
