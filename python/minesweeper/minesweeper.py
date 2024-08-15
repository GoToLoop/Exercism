def annotate(board: list[str], *, chars=''):
    if any(len(row) != len(board[0]) or row.strip(' *') for row in board):
        raise ValueError('The board is invalid with current input.')

    for r, row in enumerate(b := board.copy()):
        for c, mine_or_empty in enumerate(row):
            if mine_or_empty == '*': chars += '*'; continue

            n = sum(r[max(c-1, 0):c+2].count('*') for r in b[max(r-1, 0):r+2])
            chars += n and str(n) or ' '

        b[r] = chars; chars = ''

    return print(board, b, sep='\n') or b
