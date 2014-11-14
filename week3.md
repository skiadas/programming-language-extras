For all of these problems, it is worth-while to study the function types first. In fact the types should tell you what the function does.

* Write a function `compose_opt: ('b -> 'c option) -> ('a -> 'b option) -> 'a -> c' option` that composes two functions with "optional" values. If any of the functions returns `NONE` on its "step", the whole thing would be `NONE`.
* Write a function `do_until: ('a -> 'a) -> ('a -> bool) -> 'a -> 'a` . `do_until f p x` will apply `f` to `x` and result in a "new" `x`, and will continue to do so until `p x` is true. It will then return that `x`. Example: `do_until divBy2 isOdd n` (function names made up) will divide the number `n` by `2` until it reaches an odd number. In effect, it will remove all factors of`\`2`from`n\`.
* Using the previous function, write a function `fixed_point: (''a -> ''a) -> ''a -> ''a` that given a function `f` and an initial value `x` applies `f` to `x` until such a point where `f x = x`. (Notice the use of `''` to indicate equality types)
* Write a function `map2: ('a -> 'b) -> 'a * 'a -> 'b * 'b` that given a function that takes `'a` values to `'b` values and a pair of`\`'a`values returns the corresponding pair of`\`'b\` values. (This is fairly easy)
* Write a function `app_all: ('b -> 'c list) -> ('a -> 'b list) -> 'a -> 'c list`, so that: `app_all f g x` will apply `f` to every element of the list `g x` then concatenate the resulting `'c list`s. For example if `f n = [n, 2 * n, 3 * n]`, then `app_all f f 1 = [1, 2, 3, 2, 4, 6, 3, 6, 9]`.
* Implement [`List.foldr`](http://sml-family.org/Basis/list.html#SIG:LIST.foldr:VAL).
* Write the function `partition: ('a -> bool) -> 'a list -> 'a list * 'a list`. It should not be done via using two `filter`s, as that would require traversing the list twice. Make sure to only traverse the list once.
* Write a function `unfold: ('a -> ('b * 'a) option) -> 'a -> 'b list` that produces a list of `'b` values given a "seed" of type `'a` and a function that given a seed produces `SOME` of a pair of a `'b` value and a new seed, or `NONE` if it is done seeding. For example here is an elaborate way to count down from 5: `unfold (fn n => if n = 0 then NONE else SOME(n, n-1)) 5 = [5, 4, 3, 2, 1]`.
* You can implement `foldl` using an appropriate `foldr` on functions. (This is fairly challenging)

### Difference Lists

Many might find this one challenging.

A "difference list" is a function that "represents" a list `xs` in the following sense: Given a list `ys` as input, it returns the list `xs @ ys`, i.e. it prepends `xs` to its input. This can provide a somewhat efficient way of concatenating multiple lists, when the new lists would need to be appended on the right. Difference lists can help "delay" the computation, so that the appends can happen in their more efficient order, from right to left, instead of from left to right.

If some of this did not make sense, don't worry about it. Just think of it as practice in higher-order-functions.

First we need this new type:

    type 'a dlist = 'a list -> 'a list

So a dlist is just a function that given a list returns another list. Think of the dlist as denoting the "difference" between those two lists, in the above sense that there is some list `xs` so no matter what the input list `ys` is, the output would be the list `xs @ ys`.

Here's some functions to write:

* `toDlist: 'a list -> 'a dlist`. Takes an ordinary list, and turns it into a "dlist". So `toDlist xs` should be a function that when given a list `ys` returns `xs @ ys`.
* `append: 'a dlist -> 'a dlist -> 'a dlist`. Takes two dlists, and creates a new dlist. It should be the case, that if `f` represents the list `xs` and `g` represents the list `ys`, then `append f g` ends up being the dlist that represents the list `xs @ ys`. (But the idea is to do it without computing `xs @ ys`. You should arrange things so that the computation that would end up taking place if we did `append f g zs` would be `xs @ (ys @ zs)`.)
* `cons: 'a -> 'a dlist -> 'a dlist`. So if `f` represents the list `xs`, then `cons x f` would be the `dlist` representing `x :: xs`.
* `toList: 'a dlist -> 'a list`. Given a dlist, obtain the list it represents. This is easy.
* `concat: 'a dlist list -> 'a dlist`.

### More fun with callbacks

This requires having seen/read the "callbacks" section. In there an interface was described, to work with callbacks:

    val cbs : (int -> unit) list ref
    val onKeyEvent (int -> unit) -> unit
    val onEvent int -> unit

Where `cbs` holds a list of the callbacks to be called, `onKeyEvent` allows you to register a new callback (I will call them handlers), and `onEvent` calls all the registered callbacks. We will do various modifications to this.

The first modification, is that we will allow the handlers to return a boolean instead of `unit`. The return value is meant to indicate whether we should "stop" the propagation of the event. If a handler returns `true`, then any remaining handlers in the list must not be called for that event. Newly registered handlers must take precedence. The new signatures would be:

    val cbs : (int -> bool) list ref
    val onKeyEvent (int -> bool) -> unit
    val onEvent int -> unit

You should implement this before moving on.

For the second modification, we will allow the ability to un-register handlers. This will be done as follows: `onKeyEvent` will, instead of returning nothing, return a value that its caller could then use to remove this function from consideration. In order to do this, we need to be able to "compare" two functions, which is not possible as functions are not equality types. To get around this, we'll use a little trick: Two references are equal exactly if they are the _same_ reference. So a simple way to attach a "tag" to something to make sure you can identify it uniquely later, is to use a `ref ()`, which is a value of type `unit ref`. These don't hold any useful information, but you can test them for equality and they will only agree if they are trully the same. So with this in mind, let's define a "handler" datatype and modify our interface:

    datatype handler = Handler of (int -> bool) * unit ref
    val cbs : handler list ref
    val onKeyEvent (int -> bool) -> handler
    val removeHandler: handler -> unit
    val onEvent int -> unit

The method `removeHandler` takes a handler `Handler (f, r)` and looks through the `cbs` list looking for a handler `Handler (f', r')` whose reference is identical to `r` (i.e. `r = r'`). It then removes that handler from the list. You can decide what to do if such a handler cannot be found. You can fail silently or raise some sort of exception. Alternatively, you can have a `bool` type as the return value, an report the success or failure that way.

You should implement this before moving on.

This next part is definitely a bit more advanced. We will move on to adding "topics" to our events, essentially build a simple PubSub system. The "data' we transmit will still be single integers. Users of our system can "subscribe" their handler to either a specific topic or to all topics. Users can also use the "publish" function to publish their data on a specific topic, or all topics. Then handlers who have subscribed to those topics will be called.

We keep the callbacks for each topic in a simple list of `(topic, callbacks)`.

So here is the new interface to implement (you may find it useful to create other helper functions):

    datatype topic = AllTopics | Topic of string
    datatype handler = Handler of (int -> bool) * unit ref
    val cbs : (topic * (handler list ref)) list
    val subscribe: topic -> (int -> bool) -> handler
    val unsubscribe: handler -> unit
    val publish: topic -> int -> unit

Publishing on "AllTopics" should make all handlers fire, regardless of which specific topic they had registered for or if they had registered for "AllTopics" instead. Publishing on any topic should always fire any handlers that have subscribed to "AllTopics".

### Binomial Heaps

In this section we will build [Binomial Heaps](http://en.wikipedia.org/wiki/Binomial_heap). Some familiarity with heaps in general will be helpful. We will follow closely section 3.2 of [Okasaki's book](http://www.amazon.com/Purely-Functional-Structures-Chris-Okasaki/dp/0521663504).

This will be fairly lengthy, you have been warned.

We will build them as minheaps. The elements they will hold would be pairs, of an integer and some other element, and the integers are used for the ordering.

    type 'a elem = int * 'a

The integer may be thought of as the "weight" of the element, and it would have to be provided via some sort of weight function `weight: 'a -> int`. Given such a function, we can create elements:

    toElem: ('a -> int) -> 'a -> 'a elem
    toElem weight x = (weight x, x)

From now on we will almost always be talking about elements. You might want to create a function:

    leq: 'a elem -> 'a elem -> bool
    leq (i, _) (j, _) = i <= j

#### Binomial trees

We start with the notion of a binomial tree. A _binomial tree of rank k_ is defined recursively as follows:

1. A rank 0 binomial tree consists of a single element.
2. A rank k+1 binomial tree is formed from two rank k binomial trees by making one tree a left-most child of the other.

This is a very specific format. Let's have a look at a couple of these (and you can see more in the wikipedia entry):

1. A rank 1 binomial tree is formed from two rank 0 binomial trees, i.e. elements. So it has exactly two elements, one is the root and the other a child.
2. A rank 2 binomial tree is formed by taking two rank 1 binomial trees. The rule is that we use the root of one tree, and just add the root of the other as a new leftmost child. So that root now will have a left child that forms a rank 1 binomial tree of itself, and also will have a single element right child (a 0 rank tree). So a rank 2 binomial tree consists of a root, whose children are roots of a 1-rank and a 0-rank tree respectively. We will see this pattern continue.
3. For a rank 3 binomial tree, we need to start with two rank 2 binomial trees. Then the root of one of them will remain a root, but the root of the other will be added as a leftmost child of the first tree's root. So a rank 3 binomial tree has a root whose children are in order a 2-rank tree, a 1-tank tree and a 0-rank tree.

This holds in general: A rank r+1 binomial tree consists of a root, with r+1 children, each of which is a root for a rank r, rank r-1, rank r-2, etc binomial tree.

The number of items in a binomial tree or rank r+1 is 2^r (2 to the power r).

All the trees we will be dealing with will be heap-ordered: Every node has a value no larger than that of its children.

In SML, we can represent a tree with a triple `(rank, root, children)`:

    datatype 'a tree = T of int * 'a elem * 'a tree list

Make sure you understand how this type makes sense before moving on.

Some simple helper functions for trees, that you might want to implement (though depending on how much you use pattern matching for the rest, they may not be needed):

    rank: 'a tree -> int
    root: 'a tree -> 'a elem

We will discuss operations on such trees after we talk about heaps.

#### Binomial Heap

A _binomial heap_ is a list of binomial trees, in increasing order of rank, where no rank may be repeated (but ranks may be skipped):

    type 'a heap = 'a tree list

So for example a heap could consist of the list `[tree1, tree3]` of a rank 1 tree and a rank 3 tree. This heap will have exactly `2^3 + 2^1 = 10` elements. In general, since each tree of rank k holds 2^k elements, it follows that for every integer `N` the ranks of the trees to be used to form a heap of size N are forced upon us: They depend on the binary representation of N. For example for a heap of 21 (=16+4+1) elements we would end up using a tree of rank 0, a tree of rank 2 and a tree of rank 4, in that order. There is no other way to do it.

When a new element is to be added, these trees need to restructure themselves, and we will look at it in a moment. For the most part, all that is needed is to add lower rank trees into the mix, but some times merging needs to occur. For example if we had this heap of 21 elements, and we wanted to add one more element, then the two rank 0 trees would merge together to form a rank 1 tree.

Notice that upon seeing a binomial heap, it is not immediate which element is the smallest. You can count however on the fact that it is going to be in one of the roots that form the tree list. So it can be found in time logarithmic to the size of the heap.

We will now start building a number of useful operations. I will only describe the algorithms here, your task would be to implement them.

#### Preliminary operations

We start with operations to be used in testing that the invariants we are after are maintained. First we need two fundamental operations for building trees:

    toTree: 'a elem -> 'a tree
    treeLink: 'a tree -> 'a tree -> 'a tree

The first operation takes an element and turns it into the corresponding rank-0 tree. The second operation, `treeLink`, takes two rank-k trees and creates a rank-k+1 tree, preserving the heap ordering. So it must compare the two roots of the trees, and whichever has the largest value, it will add that tree as a leftmost child of the other tree's root. To add it as a leftmost child you simply need to cons it (`::`) to the child list. If `treeLink` is asked to link two trees of unequal ranks, it should throw an error.

Next, let us write a method that will test for a given tree if it is heap-ordered and binomial:

    isValidTree: 'a tree -> bool

It will need to test a number of things:

1. That it has the correct number of children, and they have the correct ranks.
2. That the children themselves are valid (recursive calls). (`List.all` could come in handy)
3. That the values at the roots of the children are all no less than the root of the tree (the recursive calls will take care of making sure that the elements further down the trees are even bigger).

Now we will write some functions for heaps.

    size: 'a heap -> int
    isValidHeap: 'a heap -> bool

The first function counts the number of elements in the heap. (You can either use the rank of each tree to know how many elements are in it, or you can write a size function for trees as well). The second tests if the binary heap is valid: This means that all the trees in it are "valid" according to `isValidTree`, and further that they appear in increasing rank (and no ranks are repeating).

The fundamental function you will need to write is the following:

    insertTree: 'a tree  -> 'a heap -> 'a heap

This function takes a (valid) binomial tree and a (valid) binomial heap, and merges the tree into the heap. It essentially has to walk throw the heap, and find the right spot to insert the tree. If the next tree in the list is of higher rank, then the new tree can be inserted before it, if it is of higher rank then it has to skip over it. If they are of equal rank, then the two trees need to merge (`treeLink`), producing a tree of rank one higher, and now we continue trying to insert this new tree.

Now we can discuss the main functions of a heap, which you should implement:

    emptyHeap: 'a heap        (* This is actually just a value, not a function *)
    insert: 'a elem -> 'a heap -> 'a heap
    removeMinTree: 'a heap -> 'a tree * 'a heap
    findMin: 'a heap -> 'a elem
    removeMin: 'a heap -> 'a elem * 'a heap
    heapToList: 'a heap -> 'a elem list
    heapSort: 'a elem list -> 'a elem list
    merge: 'a heap -> 'a heap -> 'a heap

The first is a value representing the empty heap (just an empty list). The function insert is given an element and a heap, in curried form, and returns the heap resulting from inserting the item into the heap. You insert an item by turning it into a rank-0 tree, then inserting that tree.

The function `removeMinTree` starts with a heap, and finds the tree whose root is the minimum element of the heap. Then it removes that tree from the list and returns a pair of this removed tree and the remaining of the heap.

The function `removeMin` takes a heap, and returns a pair of the minimum element along with the heap of the remaining elements. You need to implement this by using `removeMinTree` to get hold of the tree with the minimum element and the rest of the heap with that tree removed. Then the root of that tree is your minimum element, and the children of that root are binomial trees that need to be reinserted into the "rest of the heap" (likely via a fold and insertTree). The function `findMin` is a bit more economical, as it simply needs to return the smallest element.

The function `heapToList` forms a list of the elements in the heap in increasing order, but successively removing the smallest element. The function `heapSort` starts with a list of elements, inserts them into a heap, then calls `heapToList`. Kudos for using folds for this.

You can do variations of these last functions (and `findMin`), that work with and return `'a list`s instead of `'a elem list`s. They may need to be provided a "weight" function as an argument.

Lastly, the function `merge` merges two heaps, an operation that binomial heaps are well suited for.
