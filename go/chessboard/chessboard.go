// Declare a type named File which stores if a square is occupied by a piece.
// This will be a slice of bools

// Declare a type named Chessboard which contains a map of eight Files,
// accessed with keys from "A" to "H"

package chessboard; type File [8]bool; type Chessboard map[string]File

/*
   A B C D E F G H        1 2 3 4 5 6 7 8
 8 # _ _ _ # _ _ # 8    A # _ # _ _ _ _ # A
 7 _ _ _ _ _ _ _ _ 7    B _ _ _ _ # _ _ _ B
 6 _ _ _ _ # _ _ # 6    C _ _ # _ _ _ _ _ C
 5 _ # _ _ _ _ _ # 5    D _ _ _ _ _ _ _ _ D
 4 _ _ _ _ _ _ # # 4    E _ _ _ _ _ # _ # E
 3 # _ # _ _ _ _ # 3    F _ _ _ _ _ _ _ _ F
 2 _ _ _ _ _ _ _ # 2    G _ _ _ # _ _ _ _ G
 1 # _ _ _ _ _ _ # 1    H # # # # # # _ # H
   A B C D E F G H        1 2 3 4 5 6 7 8
 */

// CountInFile returns how many squares are occupied in the chessboard,
// within the given file.
func CountInFile(cb Chessboard, file string) (pieces int) {
	for _, occupied := range cb[file] { if occupied { pieces++ } }; return }

// CountInRank returns how many squares are occupied in the chessboard,
// within the given rank.
func CountInRank(cb Chessboard, rank int) (pieces int) {
	if rank--; 0 <= rank && rank < 8 {
		for _, slice := range cb { if slice[rank] { pieces++ } } }; return }

// CountAll should count how many squares are present in the chessboard.
func CountAll(cb Chessboard) (squares int) {
	for _, slice := range cb { for range slice { squares++ } }; return }

// CountOccupied returns how many squares are occupied in the chessboard.
func CountOccupied(cb Chessboard) (pieces int) {
	for file := range cb { pieces += CountInFile(cb, file) }; return }
