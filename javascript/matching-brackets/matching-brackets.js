// @ts-check

const BRACKETS = '{}[]()', isEven = (n=0) => !(n & 1);

export function isPaired(str='', stack='', idx=-1) { // stack of open brackets
  for (const ch of str)  if ( ~(idx = BRACKETS.indexOf(ch)) ) // found bracket
    if (isEven(idx)) stack += BRACKETS[idx]; // even is open bracket; so add it
    else if (stack.at(-1) == BRACKETS[idx - 1]) stack = stack.slice(0, -1);
    else return false; // if close bracket type isn't last index, it's a fail
  return !stack; } // all bracket pairs match if stack is empty at the end
