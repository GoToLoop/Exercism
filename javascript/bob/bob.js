// @ts-check

const AZ = /[A-Z]/, isAllCaps = (s='') => s == s.toUpperCase() && AZ.test(s);

export function hey(s='', yell = isAllCaps(s), ss='', l=console.log) {
  if (!(s = s.trim())) return "Fine. Be that way!";

  return l(s, ss = s.at(-1) != '?' ? yell && "Whoa, chill out!" || "Whatever."
  : yell && "Calm down, I know what I'm doing!" || "Sure."), ss; }
