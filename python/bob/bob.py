def response(hey_bob: str):
    hey_bob = hey_bob.strip()
    if not hey_bob: return ANSWERS[3]

    asking = hey_bob[-1] == '?'
    yelling = hey_bob.isupper()

    return (
        yelling and ANSWERS[2] or ANSWERS[0] if asking else
        yelling and ANSWERS[1] or ANSWERS[4]
    )


ANSWERS = (
    "Sure.", # 0
    "Whoa, chill out!", # 1
    "Calm down, I know what I'm doing!", # 2
    "Fine. Be that way!", # 3
    "Whatever." # 4
)
