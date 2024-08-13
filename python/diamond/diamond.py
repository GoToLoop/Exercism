def rows(ch: str, INV=slice(None, None, -1)):
    if not ch or (rows := ord(ch := ch[0].upper()) - 66) < 0: return [ ch ]

    diamond = [ 'A'.center((cols := rows + 1) * 2 + 1) ]
    mid_row = [ ch + (cols * 2 - 1) * ' ' + ch ]

    for i in range(rows):
        row = (i * ' ' + chr(i + 66)).ljust(cols)
        diamond.append(row[INV] + ' ' + row)

    diamond.extend(mid_row.extend(diamond[INV]) or mid_row)
    return print(*diamond, sep='\n') or diamond
