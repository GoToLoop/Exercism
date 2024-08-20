OCR = (' _     _  _     _  _  _  _  _ ', '| |  | _| _||_||_ |_   ||_||_|',
                                         '|_|  ||_  _|  | _||_|  ||_| _|')

ERR = 'Number of input %s is not a multiple of %s'
ROW_ERR, COL_ERR = ERR % ('lines', 'four'), ERR % ('columns', 'three')

TStr1d = tuple[str, str, str]; TStr2d = tuple[TStr1d, ...]

digitIdx = lambda num='': str(DIGITS.index(num)) if num in DIGITS else '?'

def clean(ocr: list[str]): return *(r for i, r in enumerate(ocr, 1) if i & 3),

def scanRows(ocr: TStr1d):
    return ''.join(digitIdx(scanDigit(i, ocr)) for i in size(ocr[0]))


def scanDigit(pos: int, ocr: TStr1d=OCR):
    r = slice(col := pos * 3, col + 3); return ''.join(row[r] for row in ocr)


size = lambda s='': range(len(s) // 3); DIGITS = *map(scanDigit, size(OCR[0])),

def convert(ocr: list[str]):
    if len(ocr) & 3: raise ValueError(ROW_ERR)
    if any(len(row) % 3 for row in ocr): raise ValueError(COL_ERR)

    iters = 3 * (iter(clean(ocr)),); multi: TStr2d = *zip(*iters),
    return print(*ocr, val := ','.join(map(scanRows, multi)), sep='\n') or val
