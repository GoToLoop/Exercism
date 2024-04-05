// @ts-check

/**
 * Creates a doubly-linked node object which stores a single number value.
 * @constructor
 * @param {number} v - value to store
 * @param {Node} p - previous node
 * @param {Node=} n - next node
 */
function Node(v=NaN, p=this, n) { this.v = v, this.p = p, this.n = n; }

Node.prototype.kill = function () {
  return this.p = this.n = /** @type {*} */(void 0), this; }

class Anchor { head = /** @type {Node=} */(void 0); tail = this.head; }

export class LinkedList {
  anchor = new Anchor; size = 0;

  push(v=NaN) {
    const { anchor: a } = this, newTail = (++this.size, new Node(v, a.tail));
    if (!a.head || !a.tail) return a.head = a.tail = newTail, this;
    return a.tail = a.tail.n = newTail, this; }

  pop({ anchor: a } = this) {
    if (!a.tail) return;
    const { v } = a.tail, penultNode = a.tail.p;
    return a.tail.kill(), (a.tail = penultNode).n = void 0, --this.size, v; }

  shift({ anchor: a } = this) {
    if (!a.head) return;
    const { v } = a.head, secondNode = a.head.n;
    if (secondNode) secondNode.p = a.head.p;
    return a.head.kill(), a.head = secondNode, --this.size, v; }

  unshift(v=NaN) {
    const { anchor: a } = this, newHead = new Node(v, a.head?.p, a.head);
    if (++this.size, !a.head) return a.head = a.tail = newHead, this;
    return a.head = a.head.p = newHead, this; }

  delete(v=NaN, { anchor: a } = this, node = a.head) {
    while (node) if (node.v == v) {
      if (node.n) node.n.p = node.p, node.p.n = node.n;
      else (a.tail = node.p).n = void 0;
      return node.kill(), --this.size, v; } else node = node?.n; }

  count() { return this.size; } }
