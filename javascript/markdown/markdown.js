// @ts-check

/**
 * Wraps the given text with the specified HTML tag.
 *
 * @param {*} txt - The text to wrap.
 * @param {string} tag - The HTML tag for wrapping.
 * @returns {string} The text wrapped with the HTML tag.
 *
 * @summary Changed to arrow function.
 */
const wrap = (txt, tag) => `<${tag}>${txt}</${tag}>`;

/**
 * Checks if the given text starts with a specific tag.
 *
 * @param {string} txt - The text to check.
 * @param {string} tag - The tag to look for at the beginning of the text.
 * @returns {boolean} Returns true if the text starts with the specified tag.
 *
 * @summary Changed to arrow function.
 * Removed `>` and deleted spaces from `txt` so it can match edge cases.
 * Commented it out b/c it's unused within code!
 */
// const isTag = (txt, tag) => txt['replaceAll'](' ', '').startsWith('<' + tag);

/**
 * Parses given markdown content into HTML using specified delimiter and tag.
 *
 * @param {string} markdown - The markdown content to parse.
 * @param {string} delim - The delimiter to identify content for wrapping.
 * @param {string} tag - The HTML tag to wrap the content with.
 * @returns {string} The parsed HTML content.
 *
 * @summary Changed to arrow function. Removed local variables.
 * Using wrap() to create the tag for replace()'s 2nd argument.
 */
const parser = (markdown, delim, tag) =>
  markdown.replace( RegExp(`${delim}(.+)${delim}`), wrap('$1', tag) );

/**
 * Parses the markdown content enclosed in '__' tags to HTML bold format.
 *
 * @param {string} markdown - The markdown content to parse.
 * @returns {string} The markdown content converted to HTML strong format.
 *
 * @summary Changed to arrow function.
 */
const parse__ = markdown => parser(markdown, '__', 'strong');

/**
 * Parses the markdown content enclosed in '_' tags to HTML italic format.
 * @param {string} markdown - The markdown content to parse.
 * @returns {string} The markdown content converted to HTML emphasis format.
 *
 * @summary Changed to arrow function.
 */
const parse_ = markdown => parser(markdown, '_', 'em');

/**
 * Parses the given markdown content with optional list formatting.
 *
 * @param {string} markdown - The markdown content to parse.
 * @param {boolean} isList - Indicates if the content is part of a list.
 * @returns {string} The parsed HTML content.
 *
 * @summary Changed returning if block to a ternary expression.
 */
function parseText(markdown, isList) {
  const parsedText = parse_(parse__(markdown));
  return isList ? parsedText : wrap(parsedText, 'p'); }

/**
 * Parses the header in the markdown content with optional list formatting.
 *
 * @param {string} markdown - The markdown content to parse.
 * @param {boolean} isList - Indicates if the header is part of a list.
 * @returns {[string, boolean]} A tuple containing the parsed HTML and a
 * boolean flag indicating list continuation.
 *
 * @summary Empty '' string replaces `null` value for the returning tuple.
 * Iterator `i` replaces `count` and it's a function-scoped `var` now.
 * Changed returning if block to prepend '</ul>' if `isList` expression.
 */
function parseHeader(markdown, isList) {
  for (var i = 0; i < markdown.length; ++i) if (markdown[i] != '#') break;
  if (!i || i > 6) return [ '', isList ];

  const headerHtml = wrap(markdown.slice(i + 1), 'h' + i);
  return [ (isList && '</ul>' || '') + headerHtml, false ]; }

/**
 * Parses a line item in the markdown content.
 *
 * @param {string} markdown - The markdown content to parse.
 * @param {boolean} isList - Indicates if the line item is part of a list.
 * @returns {[string, boolean]} A tuple containing the parsed HTML and a
 * boolean flag indicating list continuation.
 *
 * @summary Early return if it doesn't start w/ `*`.
 * Changed returning if block to prepend '<ul>' if not `isList` expression.
 */
function parseLineItem(markdown, isList) {
  if (markdown[0] != '*') return [ '', isList ];
  const innerHtml = wrap(parseText(markdown.slice(2), true), 'li');
  return [ (!isList && '<ul>' || '') + innerHtml, true ]; }

/**
 * Parses a paragraph in the markdown content.
 *
 * @param {string} markdown - The markdown content to parse.
 * @param {boolean} isList - Indicates if the paragraph is part of a list.
 * @returns {[string, boolean]} A tuple containing the parsed HTML and a
 * boolean flag indicating list continuation.
 *
 * @summary Cached parseText() call to a variable.
 * Changed returning if block to prepend '</ul>' if `isList` expression.
 */
function parseParagraph(markdown, isList) {
  const paragraph = parseText(markdown, false);
  return [ (isList && '</ul>' || '') + paragraph, false ]; }

/**
 * Parses a line in the markdown content.
 *
 * @param {string} markdown - The markdown content to parse.
 * @param {boolean} isList - Indicates if the line is part of a list.
 * @returns {[string, boolean]} A tuple containing the parsed HTML and a
 * boolean flag indicating list continuation.
 * @throws {Error} 'Invalid markdown' if no `result` is found among parsers.
 *
 * @summary 1-liner if blocks. Checks for an empty string instead of `null`.
 */
function parseLine(markdown, isList) {
  var [ result, inListAfter ] = parseHeader(markdown, isList);

  if (!result) [ result, inListAfter ] = parseLineItem(markdown, isList);
  if (!result) [ result, inListAfter ] = parseParagraph(markdown, isList);

  if (result) return [ result, inListAfter ];
  throw Error('Invalid markdown'); }

/**
 * Parses the markdown content into HTML.
 *
 * @param {string} markdown - The markdown content to parse.
 * @returns {string} The markdown content converted to HTML.
 *
 * @summary Moved all non-constant variables into the `for` loop as `var`.
 * Renamed `result` to `html` and `lineResult` to just `line`.
 * Removed variable `newList`. Reusing `isList` in its place.
 * Changed returning if block to append '</ul>' if last `line` `isList`.
 */
export function parse(markdown) {
  const lines = markdown.split('\n'), len = lines.length;

  for (var html = '', line = '', isList = false, i = 0; i < len; html += line)
    [ line, isList ] = parseLine(lines[i++], isList);

  return html + (isList && '</ul>' || ''); }
