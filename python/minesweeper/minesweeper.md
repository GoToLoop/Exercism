# minesweeper.py - Annotate Function

The `annotate()` function in "minesweeper.py" analyzes a list of strings
representing a Minesweeper field.

It replaces empty spaces with numbers indicating the count of adjacent mines or
keeps the empty space character if no mine is found around it.

## How It Works

1. **Input Validation**:
   - The function first checks if the input board is valid.
   - It ensures that all rows have the same length and contain only asterisks
(*) or empty spaces.
   - If the input board is invalid, it raises a `ValueError`.

2. **Board Traversal**:
   - The function iterates through each row of the board.
   - For each cell (mine or empty space) in the row:
     - If the cell contains an asterisk (`*`), which represents a mine, it keeps
the asterisk, by appending it to `chars` string.
     - Otherwise, it calculates the number of adjacent mines (within the 3x3
neighborhood centered around the cell) and appends the count (or a space if no
mines are adjacent) to `chars`.
   - After processing each row, it updates the corresponding row in a copy of
the board (`b`) with the newly created `chars`.

3. **Output**:
   - The function returns a new board (a clone of the input board) with the
updated string annotations.

4. **Example**:
   - For an input board as this:
      ```
      [
          " *  * ",
          "  *   ",
          "    * ",
          "   * *",
          " *  * ",
          "      "
      ]
      ```
   - The output board should be:
      ```
      [
          "1*22*1",
          "12*322",
          " 123*2",
          "112*4*",
          "1*22*2",
          "111111"
      ]
      ```

## Line-by-Line Explanation

- `def annotate(board: list[str], *, chars=''):`:
  - The function takes a list of strings (`board`), where each string represents
a row of it.
  - Those strings can only contain spaces or asterisks (`*`) characters.
  - Also, all strings must have the same number of characters.
  - It returns a cloned board with the annotated count of adjacent mines.
  - P.S.: Parameter `chars` is private and must be skipped when invoking the
function!

- `if any(len(row) != len(board[0]) or row.strip(' *') for row in board):`:
  - Validates the input board:
    - Checks if all rows have the same length as the first row via `len()`.
    - Ensures that each row contains only asterisks or spaces.
    - For that, it invokes `strip(' *')` expecting it to return an empty string
(which is a "falsy" value).

- `raise ValueError('The board is invalid with current input.')`:
  - Raises a `ValueError` when `any()` of the 2 checks within the generator
comprehension are "truthy". 

- `for r, row in enumerate(b := board.copy()):`:
  - Iterates through each string row of the board (using an enumerated copy of
the board, where `r` represents the current row index).

- `for c, mine_or_empty in enumerate(row):`:
  - Iterates through each cell in the row (using an enumerated row, where `c`
represents the current column index within the string row).

- `if mine_or_empty == '*': chars += '*'; continue`:
  - If the cell contains an asterisk (mine), keep it in the `chars` string, and
skip to the next inner loop iteration.

- `n = sum(r[max(c-1, 0):c+2].count('*') for r in b[max(r-1, 0):r+2])`:
  - Calculates the number of adjacent mines:
    - Slices a 3x3 neighborhood around the current cell being checked.
    - Counts the asterisks (`*` mines) within this neighborhood, using method
`count()` over a string row slice `r` inside the generator comprehension,
storing the `sum()` in variable `n`.
    - The count can vary from 0 up to max 8 adjacent mines found.
    - Built-in function `max()` ensures we avoid any slices w/ negative indices,
by clipping them back to 0. And if so, it reduces the neighborhood area by 1.
    - P.S.: The outer loop's `int r`, at the right side of the comprehension, is
temporarily overshadowed by the comprehension's own `str r` at the left side!

- `chars += n and str(n) or ' '`:
  - Appends the mine count (as a string) to `chars` string.
  - But if count `n` is 0 ("falsy"), keep the space character in `chars`.

- `b[r] = chars; chars = ''`:
  - Updates the cloned board `b` at current row index `r` with the new annotated
characters `chars`.
  - Resets the `chars` string for the next row iteration.

- `return print(board, b, sep='\n') or b`:
  - Prints the original and annotated boards (for debugging purposes).
  - Returns the annotated board.
