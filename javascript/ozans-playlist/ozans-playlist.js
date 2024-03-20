// @ts-check

/**
 * Removes duplicate tracks from a playlist.
 * @param {string[]} playlist
 * @returns {string[]} new playlist with unique entries
 */
export const removeDuplicates = playlist => [ ...new Set(playlist) ];

/**
 * Checks whether a playlist includes a track.
 * @param {string[]} playlist
 * @param {string} track
 * @returns {boolean} whether the track is in the playlist
 */
export const hasTrack = (playlist, track) => playlist.includes(track);

/**
 * Adds a track to a playlist.
 * @param {string[]} playlist
 * @param {string} track
 * @returns {string[]} same `playlist` but with `track` appended to it
 */
export const addTrack = (playlist, track) =>
  ( hasTrack(playlist, track) || playlist.push(track), playlist );

/**
 * Deletes a track from a playlist.
 * @param {string[]} playlist
 * @param {string} track
 * @returns {string[]} new `playlist` without `track`
 */
export const deleteTrack = (playlist, track, uniques=new Set(playlist)) =>
  ( uniques.delete(track), [ ...uniques ] );

/**
 * Lists the unique artists in a playlist.
 * @param {string[]} playlist in format `SONG - ARTIST`
 * @returns {string[]} list of artists
 */
export const listArtists = playlist => 
  removeDuplicates( playlist.map(entry => entry.split(' - ')[1]) );
