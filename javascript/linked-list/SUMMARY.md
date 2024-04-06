# Module LinkedList Summary

## Node Function

```javascript
function Node(v=NaN, p=this, n) {
  this.v = v, this.p = p, this.n = n;
}
```

* `v=NaN`: This sets the default value of `v` to NaN if no argument is provided.
* `p=this`: This sets the default value of `p` to the current instance (`this`)
if no argument is provided.
* `n`: This is the next node. It’s undefined if no argument is provided.
* `this.v = v, this.p = p, this.n = n`: This sets the value, previous node,
and next node of the current instance.

### Node.prototype.kill Method

```javascript
Node.prototype.kill = function () {
  return this.p = this.n = void 0, this;
}
```

* `this.p = this.n = void 0, this`: This assigns `undefined` to both ends of the
node and returns the current instance.

## Anchor Class

```javascript
class Anchor {
  head = void 0;
  tail = this.head;
  size = 0;
}
```

* `head = void 0`: This initializes the head property as undefined.
* `tail = this.head`: This sets the tail property to the same value as head.
* `size = 0`: This initializes the size property as 0.

## LinkedList Class

### push Method

```javascript
push(v=NaN) {
  const { anchor: a } = this, newTail = (++a.size, new Node(v, a.tail));
  if (!a.head || !a.tail) return a.head = a.tail = newTail, this;
  return a.tail = a.tail.n = newTail, this;
}
```

* `const { anchor: a } = this, newTail = (++a.size, new Node(v, a.tail))`: This
increments the size of the linked list, creates a new node, and assigns it to
`newTail`. `newTail` is created pointing to current tail as its previous node.
* `if (!a.head || !a.tail) return a.head = a.tail = newTail, this`: If there’s
no head or tail, it makes the new node the head and tail of the linked list.
* `return a.tail = a.tail.n = newTail, this`: This line does two main things:
  1. `a.tail.n = newTail`: It assigns the new node to the `next` property of the
  current tail. This effectively makes the new node the last node in list, by
  making the about-to-be penultimate node pointing to the newly tail node.
  2. `a.tail = newTail`: And then it updates the tail of the list to be the new
  node. This is necessary because the tail should always point to the last node
  in the linked list.

### pop Method

```javascript
pop({ anchor: a }=this) {
  if (!a.tail) return;
  const { v, p: penultNode } = a.tail;
  return a.tail.kill(), (a.tail = penultNode).n = void 0, --a.size, v;
}
```

* `if (!a.tail) return`: If there’s no tail, it just returns `undefined`.
* `const { v, p: penultNode } = a.tail`: This gets the value and previous node
of the tail.
* `return a.tail.kill(), (a.tail = penultNode).n = void 0, --a.size, v`: This
deletes the tail, makes the penultimate node the new tail, and returns the value
of the deleted node. In more details, it does these four things:
  1. `a.tail.kill()`: It removes the references to the previous and next nodes
  from the current tail, effectively isolating it from the list.
  2. `(a.tail = penultNode).n = void 0`: It updates the tail of the list to be
  the penultimate node and sets the `next` property of the new tail to
  `undefined`. This is because the tail node should not have a `next` node.
  3. `--a.size`: It decrements the size of the list by 1.
  4. `v`: It returns the value of the removed ex-tail node.

### shift Method

```javascript
shift({ anchor: a }=this) {
  if (!a.head) return;
  const { v, n: secondNode } = a.head;
  if (secondNode) secondNode.p = a.head.p;
  return a.head.kill(), a.head = secondNode, --a.size, v;
}
```

* `if (!a.head) return`: If there’s no head, it just returns `undefined`.
* `const { v, n: secondNode } = a.head`: Gets value & next node from the head.
* `if (secondNode) secondNode.p = a.head.p`: If there’s a second node, this line
updates the previous property of the second node to point to the previous node
of the current head. This is done to ensure that the second node no longer
points to the about-to-be-deleted current head node as its previous node.
* `return a.head.kill(), a.head = secondNode, --a.size, v`: This deletes the
head, makes the 2nd node the new head, decreases by 1 the number of nodes and
returns the value of the deleted ex-head node.

### unshift Method

```javascript
unshift(v=NaN) {
  const { anchor: a } = this, newHead = new Node(v, a.head?.p, a.head);
  if (++a.size, !a.head) return a.head = a.tail = newHead, this;
  return a.head = a.head.p = newHead, this;
}
```

* `const { anchor: a } = this, newHead = new Node(v, a.head?.p, a.head)`:
This increments the size of the linked list, creates a new node, and assigns it
to `newHead`. `newHead` is created pointing to current head as its next node.
* `if (++a.size, !a.head) return a.head = a.tail = newHead, this`: If there’s
no head, it makes the new node the head and tail of the linked list.
* `return a.head = a.head.p = newHead, this`: This line does two main things:
  1. `a.head.p = newHead`: It assigns the new node to the `previous` property of
  the current head. This effectively makes the new node the first node in list,
  by making the about-to-be former head node pointing to the future head node.
  2. `a.head = newHead`: And then it updates the head of the list to be the new
  node. This is necessary because the head should always point to the first
  node in the linked list.

### delete Method

```javascript
delete(v=NaN, { anchor: a }=this, node=a.head) {
  while (node) if (node.v == v) {
    if (node == a.head) return this.shift();
    else if (!node.n) return this.pop();
    node.n.p = node.p, node.p.n = node.n;
    return node.kill(), --a.size, v;
  } else node = node?.n;
}
```

* `while (node) if (node.v == v) {`: This loops through the nodes until it finds
a node with the specified value.
* `if (node == a.head) return this.shift()`: If node to be deleted is the head,
it calls the `shift` method.
* `else if (!node.n) return this.pop()`: If node to be deleted is the tail, it
calls the `pop` method.
* `node.n.p = node.p, node.p.n = node.n`: Otherwise, it updates the next and
previous nodes of the node to be deleted, so they become interconnected.
* `return node.kill(), --a.size, v`: This deletes the node, decrements the size
of the linked list and returns the value of the deleted node.
* `else node = node.n`: Next node is now current node to search for the value.
If there's no next node, the while loop will abort and result will be undefined.

### count Method

```javascript
count() { return this.anchor.size; }
```

* `return this.anchor.size`: Returns current number of nodes in the linked list.
