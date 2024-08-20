# OCR Digit Recognition Solution

This Python code provides a solution for recognizing digits represented in a 3x4 grid of pipes, underscores, and spaces.
The goal is to convert these visual representations into decimal numbers.

## Code Explanation

1. **Input Representation (OCR Grid)**
    - The input is a list of strings (`ocr[]`), where each string represents a row of the grid.
    - The grid consists of three rows, and each row can contain these characters: pipes, underscores, or spaces.

2. **Error Handling**
    - The code checks if the number of input lines is a multiple of four (to ensure a valid grid).
    - It also verifies that each row has a length divisible by three (to ensure valid columns).

3. **Cleaning the Input**
    - The `clean()` function removes any unnecessary blank lines from the input.
    - It keeps only the rows that correspond to actual digits.

4. **Scanning Rows and Digits**
    - The `scanRows()` function processes each row to identify the digit it represents.
    - It calls the `scanDigit()` function for each position (1, 2, 3) within the row.
    - The `scanDigit()` function extracts the 3x3 representation of a digit at a specific position.

5. **Digit Indexing**
    - The `digitIdx()` function maps the scanned digit to its index (0-9) in the `DIGITS` tuple as a string.
    - If the digit is not recognized, it returns a question mark (`?`).

6. **Conversion and Output**
    - The `convert()` function combines the scanned digits from all rows.
    - It joins the recognized digits with commas to form the final output string.
    - The result is printed and returned.

### Line-by-Line Explanation:

1. **`OCR` Definition:**
    ```py
    OCR = (' _     _  _     _  _  _  _  _ ',
           '| |  | _| _||_||_ |_   ||_||_|',
           '|_|  ||_  _|  | _||_|  ||_| _|')
    ```
   - `OCR` is a tuple of three strings representing the visual representation of digits (0-9) using pipes, underscores, and spaces.
   - Each string corresponds to a row in the 3x3 grid.

2. **Error Handling Constants (`ERR`, `ROW_ERR`, `COL_ERR`):**
    ```py
    ERR = 'Number of input %s is not a multiple of %s'
    ROW_ERR, COL_ERR = ERR % ('lines', 'four'), ERR % ('columns', 'three')
    ```
   - These constants define error messages related to input validation.
   - `ROW_ERR` indicates an incorrect number of input lines (when not divisible by 4).
   - `COL_ERR` indicates invalid column lengths (when not divisible by 3).

3. **Type Aliases (`TStr1d`, `TStr2d`):**
    ```py
    TStr1d = tuple[str, str, str]
    TStr2d = tuple[TStr1d, ...]
    ```
   - These type aliases represent one-dimensional and two-dimensional tuples of strings, respectively.

4. **`digitIdx()` Function:**
    ```python
    digitIdx = lambda num='': str(DIGITS.index(num)) if num in DIGITS else '?'
    ```
   - Maps a scanned digit (from invoking `scanDigit()` in `scanRows()`) to its index (0-9) in `DIGITS`.
   - First, it checks if `num` is present in the `DIGITS` tuple (which contains all 10 pre-scanned digits).
   - If found, it returns the corresponding index of `num` in the `DIGITS` tuple (converted to a string).
   - Returns a question mark (`'?'`) if the digit is unrecognized.

5. **`clean()` Function:**
    ```python
    def clean(ocr: list[str]):
        return *(r for i, r in enumerate(ocr, 1) if i & 3),
    ```
   - Removes blank lines from the input `ocr[]`, keeping only relevant rows that form the shape of the digits.
   - It uses a generator expression to iterate over the input rows `ocr[]` with `enumerate()` starting at count 1.
   - Then it filters out every fourth row `r` (when `i & 3` is 0) and returns the remaining rows as a tuple of strings.

6. **`scanRows()` Function:**
    ```python
    def scanRows(ocr: TStr1d):
        return ''.join(digitIdx(scanDigit(i, ocr)) for i in size(ocr[0]))
    ```
   - Processes each row from a tuple of three strings `ocr[]` to identify the digit it represents.
   - Iterates over each digit position in `ocr[]` using `size()` to determine how many digits there are.
   - For each position `i`, it calls `scanDigit()` to get the digit's visual representation within `ocr[]`.
   - Then maps it to its index using `digitIdx()`.
   - Joins the results into a single string and returns it.

7. **`scanDigit()` Function:**
    ```python
    def scanDigit(pos: int, ocr: TStr1d=OCR):
        r = slice(col := pos * 3, col + 3)
        return ''.join(row[r] for row in ocr)
    ```
   - Extracts the 3x3 character representation of a digit at a specific position `pos`.
   - Uses the `pos` parameter to determine the offset position among the 3 rows of `ocr[]` for slicing.
   - **`r = slice(col := pos * 3, col + 3)`**: Calculates the starting column index `col` as `pos*3` and creates a slice `r` from `col` to `col+3`.
   - **`return ''.join(row[r] for row in ocr)`**: Extracts the slice `r` from each `row` in `ocr[]`, joins the slices into a single string, and returns it.

8. **`size()` Function:**
    ```py
      size = lambda s='': range(len(s) // 3)
    ```
   - Calculates the number of 3x3 digit representations that fit in the string row `s` and returns it as a range.

9. **`DIGITS` Tuple:**
    ```py
      DIGITS = *map(scanDigit, size(OCR[0])),
    ```
   - Creates a tuple of 3x3 visual representation of scanned digits (0-9) using `scanDigit()` to generate each digit mapped from the `size()` range of the OCR tuple.
   - In short, it turns OCR, which is a tuple of 3 literal strings, into a tuple of 10 strings representing the shapes of 0 to 9 as DIGITS.

10. **`convert()` Function:**
    ```python
    def convert(ocr: list[str]):
        if len(ocr) & 3: raise ValueError(ROW_ERR)
        if any(len(row) % 3 for row in ocr): raise ValueError(COL_ERR)

        iters = 3 * (iter(clean(ocr)),)
        multi: TStr2d = *zip(*iters),

        return print(*ocr, val := ','.join(map(scanRows, multi)), sep='\n') or val
    ```
    - The module file's main function that solves the OCR challenge.
    - Checks for valid row and column counts, raising errors if the input is invalid.
    - Cleans the input and combines scanned digits from all rows.
    - Joins recognized digits with commas to form the final output string.
    - **`if len(ocr) & 3: raise ValueError(ROW_ERR)`**: Checks if the number of rows in `ocr[]` is not a multiple of 4. If so, raises a `ValueError` with the message `ROW_ERR`.
    - **`if any(len(row) % 3 for row in ocr): raise ValueError(COL_ERR)`**: Checks if any string `row` in `ocr[]` has a length not divisible by 3. If so, raises a `ValueError` with the message `COL_ERR`.
    - **`iters = 3 * (iter(clean(ocr)),)`**:
      - Calls `clean()` to remove every fourth row from the `ocr[]` list input, which are blank rows anyways.
      - Creates an iterator from the cleaned input, which is now a tuple of strings, via built-in `iter()`.
      - Multiplies the iterator by 3, creating a tuple of three aliases for the same iterator object. This clever trick makes it so that when built-in `zip()` accesses these iterators, it will pick the next string where the previous left off.
    - **`multi: TStr2d = *zip(*iters),`**:
      - Uses `zip()` to group the iterators into tuples of three row strings each, forming a line of digits.
      - The `*iters` unpacks the tuple of 3 alias iterators, and `zip(*iters)` effectively groups the cleaned lines into sets of three, b/c the length of a tuple created by `zip()` is equal to the number of passed iterable arguments.
      - The result is a 2D tuple `multi[][]`, where each element is a tuple of three row strings, representing a line of digit shapes within it.
    - **`return print(*ocr, val := ','.join(map(scanRows, multi)), sep='\n') or val`**:
      - Maps `scanRows()` over `multi[][]` to convert each set of three string rows (1 line) into recognized digits.
      - Joins the results using commas into a single string `val`.
      - Prints the original `ocr[]` input and the result `val`, separated by newlines.
      - Returns the cached result `val`.

## Example Usage

Given the following OCR grids:

```
convert([
    "    _  _     _  _  _  _  _  _ ",  # 
    "  | _| _||_||_ |_   ||_||_|| |",  # decimal numbers
    "  ||_  _|  | _||_|  ||_| _||_|",  # 
    "                              "   # fourth line is always blank
])
```

The output is "1234567890".

```
convert([
    "       _     _           _ ",
    "  |  || |  || |     || || |",
    "  |  | _|  ||_|  |  ||_||_|",
    "                           "
])
```

The output is "11?10?1?0"

```
convert([
    "    _  _ ",
    "  | _| _|",
    "  | _||_ ",
    "         ",
    "    _  _ ",
    "|_||_ |_ ",
    "  | _||_|",
    "         ",
    " _  _  _ ",
    "  ||_||_|",
    "  ||_| _|",
    "         "
])

```

The output is "123,456,789".

---
