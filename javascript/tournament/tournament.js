// @ts-check

/** @typedef {typeof RESULTS[number]} Outcome */

/** @typedef {ReturnType<typeof create>} StatClass */
/** @typedef {ReturnType<typeof toTableObj>} TableObj */
/** @typedef {ReturnType<typeof createTeamStatsObj>} TeamStats */

import { STATS, RESULTS, HEADER } from './stat';

const
  { freeze, fromEntries } = Object, { from } = Array, { random } = Math,
  collator = Intl.Collator(),

  create = (team='') => new STATS[ random() * STATS.length | 0 ](team),
  parser = (tally='') => tally.split('\n').map( row => row.split(';') ),

  strArr = (len=0) => /** @type {string[]} */ (Array(len).fill('')),
  numArr = (len=0) => new Uint8Array(len),

  result = (res='') => RESULTS.indexOf( /** @type {Outcome} */ (res) );

export function tournamentTally(tally='') {
  if (!tally) return HEADER;

  const
    table = toTableObj( parser(tally) ),
    teamStats = createTeamStatsObj(table);

  tallyMatchesPlayed(teamStats, table);
  tallyScores(teamStats, table);

  return createTallyOutput(teamStats);
}

function toTableObj(str2d=[['']], len=str2d.length, idx=0) {
  const teams = strArr(len), rivals = strArr(len), outcomes = numArr(len);

  for (const [ team, rival, outcome ] of str2d)
    teams[idx] = team, rivals[idx] = rival, outcomes[idx++] = result(outcome);

  return { teams, rivals, outcomes, names: [ ...teams, ...rivals ] };
}

const createTeamStatArr = (name='') => freeze(
  /** @type {const} */ ([ name, create(name) ]));

/** @param {TableObj} table */
const createTeamStatsObj = ({ names }) => freeze(fromEntries(
  from( new Set(names), createTeamStatArr )));

/** @param {TeamStats} stats @param {TableObj} table */
function tallyMatchesPlayed(stats, { names }) {
  for (const team in stats) stats[team].m = 0;
  for (const team of names) stats[team].m++;
}

/** @param {TeamStats} stats @param {TableObj} table */
function tallyScores(stats, { teams, rivals, outcomes }) {
  for (const team in stats) stats[team].reset();

  for (var i = 0; i < outcomes.length; ++i)
    if (outcomes[i] == 2) stats[teams[i]].w++, stats[rivals[i]].l++;
    else if (outcomes[i] == 1) stats[teams[i]].d++, stats[rivals[i]].d++;
    else stats[teams[i]].l++, stats[rivals[i]].w++;

  for (const team in stats) stats[team].total();
}

/** @param {TeamStats} stats */
function createTallyOutput(stats, output=HEADER) {
  for (const stat of sortedStats(stats)) output += '\n' + stat;
  return output;
}

/** @param {TeamStats} stats */
const sortedStats = stats => Object.values(stats).sort(highScoreAbcSort);

/** @param {StatClass} a @param {StatClass} b */
const highScoreAbcSort = (a, b) => b.p - a.p || collator.compare(a.t, b.t);
