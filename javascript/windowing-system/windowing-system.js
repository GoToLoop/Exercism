// @ts-check

/////////////////////////////////// clamp() ////////////////////////////////////
/**
 * Clamps a number between two values.
 * @param {number} num the input number
 * @param {number} min the lower boundary
 * @param {number} max the upper boundary
 * @returns {number} the clamped value
 */
export const clamp = (num, min, max) => Math.min( Math.max(num, min), max );
///////////////////////////////////// Size /////////////////////////////////////
/** @constructor */
export function Size(w=80, h=60) { this.width = w, this.height = h; }

/**
 * @param {number} newWidth
 * @param {number} newHeight
 */
Size.prototype.resize = function (newWidth, newHeight=newWidth) {
  return this.width = newWidth, this.height = newHeight, this; }
/////////////////////////////////// Position ///////////////////////////////////
/** @constructor */
export function Position(x=0, y=0) { this.x = x, this.y = y; }

/**
 * @param {number} newX 
 * @param {number} newY 
 */
Position.prototype.move = function (newX, newY=newX) {
  return this.x = newX, this.y = newY, this; }
//////////////////////////////// ProgramWindow /////////////////////////////////
export class ProgramWindow {
  screenSize = new Size(800, 600); size = new Size; position = new Position;

  resize(newSize=new Size) {
    const { width, height } = newSize, { screenSize, size, position } = this;

    return size.resize(clamp(width,  1, screenSize.width  - position.x),
                       clamp(height, 1, screenSize.height - position.y)), this;}

  move(newPos=new Position) {
    const { x, y } = newPos, { screenSize, size, position } = this;

    return position.move(clamp(x, 0, screenSize.width  - size.width),
                         clamp(y, 0, screenSize.height - size.height)), this; }}
//////////////////////////////// changeWindow() ////////////////////////////////
export const changeWindow = (win=new ProgramWindow) =>
  ( win.size.resize(400, 300), win.position.move(100, 150), win );
  //win.resize( win.size.resize(400, 300) ).move( win.position.move(100, 150) );
////////////////////////////////////////////////////////////////////////////////
