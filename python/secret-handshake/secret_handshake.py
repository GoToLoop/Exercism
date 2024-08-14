ACTIONS = { 1: 'wink', 2: 'double blink', 4: 'close your eyes', 8: 'jump' }

def commands(binary: str):
    secret = int('0b' + binary, 0); actions = [''] * 0
    for mask in ACTIONS: secret & mask and actions.append(ACTIONS[mask])
    secret & 16 and actions.reverse(); return print(binary, actions) or actions
