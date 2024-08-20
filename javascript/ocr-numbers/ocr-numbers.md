# OCR Numbers JavaScript File Explanation

This JS file is about a challenge for a very simplified concept for an Optical Character Recognition (OCR) of number digits inside a string, composed of pipes, spaces and underscores.

## Line-by-Line Code Explanation

  * **`// @ts-check`**: This line enables TypeScript checking in JavaScript files when using Visual Studio Code as the editor.

  * **`const { freeze: icy } = Object, { from } = Array, SPC = / +$|\n/g`**: This line declares three constants:

     - `icy()`: An alias to the `Object.freeze()` static method, which makes an object immutable, like a tuple.
     - `from()`: An alias to the `Array.from()` static method, which creates a new array instance from an iterable object.
     - `SPC`: A regular expression that matches one or more space characters at the end of a string and newlines anywhere.

  * **`clean = (ocr='') => ocr.replace(SPC, '')`**: This function removes trailing spaces and all newline characters from the input string using the `replace()` method by passing the SPC regular expression. Be aware that if there are any spaces just before newlines characters, the result string will end with those spaces as trailing characters. However, that's an intentional behavior, b/c an OCR digit representation such as "2" requires the preservation of its ending space character! Notice also that a simpler regex such as `/ +$/` would be just enough for the OCR code to work, b/c the way `scanDigit()` is implemented it can tolerate some trailing invalid newlines when interpreting an OCR.

  * **`digits = (s='') => s.length/12 | 0`**: This function takes a string as input and returns the integer division of its `length` by 12. This is used to calculate the expected number of digits that can be found in the OCR input, where each digit is formed by 3 x 4 (cols x rows) characters. However, it's crucial to use `digits()` before `clean()`, b/c the latter would shorten the shape of the digits to 3 x 3, given the discarded 4th row in its original form is just space characters.

  * **`digitIdx = (num='', idx=0) => ~(idx = DIGITS.indexOf(num)) ? idx : '?'`**: This function takes a 3 x 3 digit string representation `num` as input. Then it searches the DIGITS array for that digit string shape and caches it in `idx`. If the digit string is found, it returns that cached `idx`; otherwise, it returns `'?'`, b/c the unary bitwise NOT operator `~` would turn `-1`, which is the value returned by the array method `indexOf()` for not found, into `0` (and vice-versa); and the latter is a "falsy" value in JS.

  * **`multiScan = (ocr='') => icy(ocr.split(REGEX).map((s, _, __, c=clean(s)) => icy(from({ length: digits(s) }, (_, i) => scanDigit(i, c)))))`**: This function takes an `ocr` string as input, splits it into 3-col x 4-row string blocks using the REGEX regular expression, loops over each of those blocks as parameter `s` inside method `map()`, which will convert each block into an array of strings. Inside `map()` now, it creates a sub-array for each `s` block using `from()` in order to store each individual digit found in each block, calculating their expected quantity via `digits()`. Now inside `from()`'s callback, it re-uses the previously cached `c` parameter that holds a cleaned up `s` block via `clean()`, which is now in a 3-col by 3-row string shape; passing it as the 2nd argument to `scanDigit()`, which in turn will scan and find out each 3 x 3 digit in the block within the index `i` "area". Finally, the `multiScan()` function returns a fully "frozen" array of arrays of string digit shapes. P.S.: The `icy()` function is used to make the arrays immutable.

  * **`REGEX = /(?<=\n\s{3,})\n/`**: This is a regular expression used to split the OCR input into separate blocks. It matches a newline character `\n` that is preceded by another newline character followed by at least three space characters. The `(?<=...)` syntax is a positive lookbehind assertion, which means the match must be preceded by the pattern inside the parentheses, but that pattern is not included in the match. Thus the `split()` within `multiScan()` will only consume the last newline character delimiter outside the lookbehind. So the other characters inside the assertion, including the 1st newline, are kept in the left part of the split strings.

  * This is the compact 3 x 3 OCR representation of the digits 0-9 sans the 4th row, which is merely space characters:

    ```
    OCR = ' _     _  _     _  _  _  _  _ ' +
          '| |  | _| _||_||_ |_   ||_||_|' +
          '|_|  ||_  _|  | _||_|  ||_| _|'
    ```

  * **`DIGITS = icy(from({ length: 10 }, (_, i) => scanDigit(i)))`**: DIGITS is an immutable string array representing each 3 x 3 digit shape from 0 to 9 converted from the constant OCR string using `scanDigit()`.

  * **`function scanDigit(pos=0, ocr=OCR) {...}`**: This function scans 3 x 3 digits from an `ocr` input and builds & returns the corresponding digit shape within the specified "area" position `pos`. It does this by iterating over the `ocr` input in steps of `cols` (the number of columns of each row in the `ocr` input, based on its length `len` divided by 3), starting at the specified `idx` position `pos` * 3 (which acts as an offset) and concatenating the substrings of length 3 at each step to form the digit string:

    - **`const len = ocr.length, cols = len/3 | 0, idx = pos*3;`**: This line declares three constants:

       1. len: The length of the `ocr` string, which `cols` will be based on.
       2. cols: The number of columns of each row in the OCR input, calculated as the integer division of `len` by 3.
       3. idx: The starting index for scanning the digit, calculated as parameter `pos` * 3.

    - **`for (var digit='', i = idx; i < len; i += cols) digit += ocr.substr(i, 3);`**: This for loop iterates over the `ocr` string in steps of `cols`, starting at `idx`. In each iteration, it appends a substring of fixed length 3, starting at the current index `i`, to the string variable `digit` via "legacy" method `substr()`.

    - **`return digit;`**: This line returns the final `digit` string, which is the OCR shape representation of the digit at position `pos`.

  * **`export function convert(ocr=OCR, blocks = multiScan(ocr)) {...}`**: This function converts the `ocr` input into a string of digit characters. It does this by iterating over the `blocks` of digit shapes returned by `multiScan()`, which is a 2d array of strings; then converting each `digit` to its index value in the DIGITS array using `digitIdx()`, and concatenating the results with commas in between:

    - **`for (var val = '', i = 0; i < blocks.length; val += ',')`**: This for loop iterates over the `blocks` array. In each iteration, it appends a comma to the `val` string.

    - **`for (const digit of blocks[i++]) val += digitIdx(digit);`**: This nested for loop iterates over the `i`-th line of `blocks`. In each iteration, if found, it appends the index of the current `digit` in the DIGITS array to the `val` string; otherwise it appends '?'.

    - **`return console.log(ocr + '\n', val = val.slice(0, -1)), val; }`**: This line logs & returns the `val` string without the trailing comma. This is the final output of the function, which is a string of digit characters (0-9) separated by commas. The numerical characters are the indices of its OCR representations from the digit shapes inside the DIGITS array.

## Examples

  ```
  convert(
    ' _ \n' +
    ' _|\n' +
    '|_ \n' +
    '   ')
  ```
  should output '2'
  ```
  convert(
    '       _     _           _ \n' +
    '  |  || |  || |     || || |\n' +
    '  |  | _|  ||_|  |  ||_||_|\n' +
    '                           ')
  ```
  should output '11?10?1?0'
  ```
  convert(
    '    _  _ \n' +
    '  | _| _|\n' +
    '  ||_  _|\n' +
    '         \n' +
    '    _  _ \n' +
    '|_||_ |_ \n' +
    '  | _||_|\n' +
    '         \n' +
    ' _  _  _ \n' +
    '  ||_||_|\n' +
    '  ||_| _|\n' +
    '         ')
  ```
  should output '123,456,789'
