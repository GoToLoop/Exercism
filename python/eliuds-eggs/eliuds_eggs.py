# egg_count = lambda coop=0: coop and egg_count(coop >> 1) + (coop & 1)
egg_count = lambda coop=0: coop and 1 + egg_count(coop & (coop - 1))
