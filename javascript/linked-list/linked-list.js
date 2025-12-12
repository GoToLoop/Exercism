// @ts-check

/**
 * Creates a doubly-linked node object which stores a single number value.
 * @constructor
 * @param {number} v - value to store
 * @param {Node} p - previous node
 * @param {Node=} n - next node
 */
function Node(v=NaN, p=Node.N, n) { this.v = v, this.p = p, this.n = n; }

Node.prototype.kill = function () { // assigns undefined to both node ends
  return this.p = this.n = /** @type {*} */(void 0), this; }; Node.N = new Node;

class Anchor {head = /** @type {Node=} */(void 0); tail = this.head; size = 0;}

export class LinkedList {
  anchor = new Anchor; // container for 1st & last nodes, plus total of nodes

  push(v=NaN) { // appends a new value to the tail of the linked list
    const { anchor: a } = this, newTail = (++a.size, new Node(v, a.tail));
    if (!a.head || !a.tail) return a.head = a.tail = newTail, this;
    return a.tail = a.tail.n = newTail, this; } // makes new node the last node

  pop({ anchor: a }=this) { // deletes & returns the value of the last node
    if (!a.tail) return; // just returns undefined if there's no last node
    const { v, p: penultNode } = a.tail; // penultimate node becomes last node 
    return a.tail.kill(), (a.tail = penultNode).n = void 0, --a.size, v; }

  shift({ anchor: a }=this) { // deletes & returns the value of the 1st node
    if (!a.head) return; // just returns undefined if there's no 1st node
    const { v, n: secondNode } = a.head; // 2nd node will become 1st node

    // 2nd node no longer points to the about-to-be-deleted current head node:
    if (secondNode) secondNode.p = a.head.p; // do that only if 2nd node exists 
    return a.head.kill(), a.head = secondNode, --a.size, v; }

  unshift(v=NaN) { // prepends a new value to the head of the linked list
    const { anchor: a } = this, newHead = new Node(v, a.head?.p, a.head);
    if (++a.size, !a.head) return a.head = a.tail = newHead, this;
    return a.head = a.head.p = newHead, this; } // makes new node the 1st node

  delete(v=NaN, { anchor: a }=this, node=a.head) {
    while (node) if (node.v == v) { // node containing deleting value found!
      if (node == a.head) return this.shift(); // deleting head/1st node
      else if (!node.n) return this.pop(); // deleting tail/last node

      // Next node points to previous node of current node and vice-versa: 
      node.n.p = node.p, node.p.n = node.n;
      return node.kill(), --a.size, v; } // kill match node and decrease count

    else node = node.n; } // searching for next node until 1 is undefined

  count() { return this.anchor.size; } } // returns current number of nodes
