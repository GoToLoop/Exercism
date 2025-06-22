package electionday; import "fmt"

// NewVoteCounter returns a new vote counter w/ a given number of initial votes.
func NewVoteCounter(initialVotes int) *int { return &initialVotes }

// VoteCount extracts the number of votes from a counter.
func VoteCount(votes *int) int { if votes != nil { return *votes }; return 0 }

// IncrementVoteCount increments the value in a vote counter.
func IncrementVoteCount(votes *int, increment int) { *votes += increment }

// NewElectionResult creates a new election result.
func NewElectionResult(candidateName string, votes int) *ElectionResult {
	return &ElectionResult{ candidateName, votes } }

// DisplayResult creates a message with the result to be displayed.
func DisplayResult(result *ElectionResult) string {
	return fmt.Sprintf("%s (%d)", result.Name, result.Votes) }

// DecrementVotesOfCandidate decrements by 1 a candidate's vote count in a map.
func DecrementVotesOfCandidate(votes map[string]int, c string) { votes[c]-- }
