
### More fun with references

1. (This was sort of already done in week 3's notes) Write a function `makeCounter: int -> unit -> int`, with the following behavior: `makeCounter c` returns a function, say `f`, that when called returns successively increasing numbers starting from `c`. So the first `f()` returns `c`, the second `f()` returns `c+1` and so on.
2. Write a function `makeMultiCounter: unit -> string -> int`, with the following behavior: `makeMultiCounter()` returns a function that takes as inputs strings and returns integers. This function, call it `f`, should behave as follows: For any string `s`, each time `f s` is called it will return an ever increasing number, starting from `1`. So the first call to `f "x"` produces `1`, the second call to `f "x"` produces `2` and so on. A call to `f "y"` would return 1, but some later call to `f "y"` would return `2`. In other words, `f` keeps a separate count for each string input. Each call of `makeMultiCounter()` should be creating a distinct such counting function. No other top-level bindings should be created.
3. Write a version of `makeMultiCounter`, where multiple calls to `makeMultiCounter ()` all return the same counting function. Again, no other top-level bindings should be created.
4. Write a function `gen: ('a -> 'a) -> 'a -> (unit -> 'a)` that given a function `f` and an initial "seed" value `s`, returns a function `g` so that when `g` is called, the result `f s` will be computed and returned, and that return value will be used a the input to `f` the next time `g` is called. For example if `f = fn x => x * x` and `g = gen f 2`, then `g() = 4` the first time, `g() = 16` the second time and so on. Effectively, `g` will be generating the numbers `f(s)`, `f(f(s))`, `f(f(f(s)))` and so on.
5. Use `gen` to create the counter in the first problem.
6. Write a function `once: (unit -> 'a) -> 'unit -> 'a` that is given a function `f`, and behaves as follows: If `g` is equal to `once f`, then calling `g ()` will only result in calling `f` once, the first time. Every subsequent call should simply return the resulting value, without running `f` again. `f` must not be called until `g` is called for the first time. No new top-level bindings or datatypes should be introduced.
7. Write similarly a function `only: int -> (unit -> 'a) -> 'unit -> 'a` that is given an integer `n` and a function `f`, and returns a function that for the first `n` times it is called it will call `f`, and from that point on it will recycle the `n` results computed thus far. For example, if `f` is the counter function from the earlier problems, and `g = only 3 f`, then calling `g` 5 times would result in the sequence of results `1,2,3,1,2`.
8. Write a function `cache: ('a -> 'b) -> ('a -> 'b)` that given a function `f` returns a function `g` that "caches" `f`'s computations. On a call to `g` with some input, if it is the first time that `g` has been call with that specific input, `f` should be called on that input and the result cached. Subsequent calls to `g` with the same input should not recompute `f`, and instead use the cached value. You can use a simple list of pairs to hold the cached values (in the absense of some ordering structure on `'a`, not much more is possible).
9. (Harder) Write a version of `cache: int -> ('a -> 'b) -> ('a -> 'b)` that takes as a first argument a cache size `n`. It will then cache up to `n` values. On subsequent cache misses you will need to remove a "stale" value from the cache. How you define "stale" is up to you, you can try to keep track of frequency of calls, "recency" using a counter etc. Finding a way to do this efficiently, so that you don't always end up removing the last element on a list and requiring a reconstruction of the list, may be challenging.
10. Write a function `throttle: int -> ('a list -> unit) -> ('a -> unit)` that is given an integer `n` and a function `f`, and returns a function `g` that "throttles" `f` in the following sense: As `g` is called, its inputs are "accumulated" into a list. On the `n` call, this list of `n` elements is passed as an argument to `f` and the process starts afresh. So `f` will be called on every `n`-th call of `g`, and it will be given as argument the list of the arguments that `g` had in the last `n` calls. You must preserve the order of those arguments.
11. Write a function `throttle2: int -> ('a -> unit) -> ('a -> unit)` that acts as follows: The first time `g` is called, its input is stored, and nothing else happens until `g` is called for the `n`-th time. When `g` is called for the `n`-th time, it calls `f` with the stored argument, and the process starts anew (so the `n+1`-st argument will be stored, but `f` won't be called on it until the `2n`-th call to `g`, and so on.
12. Write a function `delayed: ('a -> 'b) -> 'a -> ('a -> 'b)` that acts as follows: If `g = delayed f x0` then the call `g x1` will return `f x0`, the call `g x2` will return `f x1` and so on. Namely each time `g` is called, it in turn calls `f` with the argument that it was given in its _previous_ call, using `x0` for the first time it's called.
13. There is a built in operator `before` which behaves as follows: `e1 before e2` evaluates `e1`, then evaluates `e2` but returns the value of `e1`. The built in version has type `'a * unit -> 'a`. Define a similar infix operator `befor: 'a * 'b -> 'a`. (This is very very easy once you think about it).

### A simple hash table

We will implement a simple hash table with linked list chaining. This is probably not the best way to arrange it, but it will be good for practice.

We start with the datatypes

    type 'a hashVector = 'a list ref vector
    type ('a, 'b) hashTable = { hash: 'a -> int, eq: 'a * 'a -> bool, size: int, vec: ('a * 'b) hashVector }

So a hashTable consists of: a hashing function, a function to compare two values for equality, and a hashVector. A hashVector is a vector (immutable fixed width array ([](http://sml-family.org/Basis/vector.html)[](http://sml-family.org/Basis/vector.html)[](http://sml-family.org/Basis/vector.html)[http://sml-family.org/Basis/vector.html](http://sml-family.org/Basis/vector.html))) whose entries are references to linked lists. Upon creation we populate the vector with empty list references. Then each time a new value needs to be hashed, its proper assigned location in the vector is found via the hash function, then the list stored in that reference is found, the element is appended to that list, or replaced if the key already exists, then the new list is stored in the reference.

Here are some methods you should implement:

    makeEmpty: ('a -> int, 'a * 'a -> bool, int) -> ('a, 'b) hashTable

This accepts a hash function, and equality function, and a size integer, and creates an empty hashTable of that size. The function `Vector.tabulate` will come in handy for creating the necessary vector. This is the only function that creates a new hashTable, all other functions will update an existing hashTable.

Next we will need some methods to retrieve/update key-value pairs from a list. These are needed once you have drilled down to the correct hash location and now need to update the list that is there.

    lookup_list: ('a * 'a -> bool) * ('a * 'b) list -> 'a -> 'b option
    insert_list: ('a * 'a -> bool) * ('a * 'b) list -> ('a, 'b) -> ('a * 'b) list
    remove_list: ('a * 'a -> bool) * ('a * 'b) list -> 'a -> ('a * 'b) list

The first function looks up for a particular key in the list, using the provided function (probably eq) as argument, and if it finds it then returns `SOME v`, where `v` is the corresponding value.

The second function inserts the new key-value pair at the end of the list if that key doesn't already exist. If there is a key-value pair with the same key, then it gets replaced.

The third function, predictably, removes a key if it is present.

Now we can write the corresponding function at the level of the hashTable:

    lookup: ('a, 'b) hashTable -> 'a -> 'b option
    insert: ('a, 'b) hashTable -> 'a * 'b -> unit
    remove: ('a, 'b) hashTable -> 'a -> unit

The first function looks up a key in the hashTable, the second inserts a new key-value pair into the hashTable. Your implementations would have to use the hash function to locate the `list ref` in which this pair should be looked in/inserted, then the corresponding list function would be used. In the case of insert/remove, the contents of the ref need to be updated with the new list.

    load: ('a, 'b) hashTable -> int

Computes how many key-value pairs are currently stored in the hashTable. With our current implementation of hashTables this will not be particularly efficient. You may change the hashTable definition to maintain the load information in an `int ref` field, and adjust the methods accordingly (note that insert may replace/add depending on whether the key is present, and remove many not actually remove).

### A simple calculator

This example illustrates mutual recursion in both types and functions. We will create a basic datatype that supports arithmetic expressions as well as statements for storing and printing variables. For that we need one datatype for expressions and one for statements, and they depend on each other. So here is one attempt:

    datatype Exp = Add of Exp * Exp
                 | Sub of Exp * Exp
                 | Mult of Exp * Exp
                 | Const of int
                 | Var of string
                 | Compound of Stm list * Exp
    and      Stm = Assign of string * Exp
                 | Print of Exp

For simplicity we will assume that any assignments that happen within the statements in a compound expression are local to that expression. So think of a compound statement as a `let-in-end` block.

In order to manage the assignments, we will maintain a "calculator memory" in the form of a `(string * int) list`. Each assignment adds a "binding" in this memory, each `Var` expression accesses the memory. To manage this we will need some minor definitions, that you should be able to come up with:

    exception UnboundVar of string
    type memory = (string * int) list
    save: memory -> string * int -> memory
    load: memory -> string -> int

Now we want to implement an "evaluation" strategy. It will need to be a pair of functions:

    evalExp: memory -> Exp -> int
    evalStm: memory -> Stm -> memory

Each of these functions requires as input the current state of the memory, as well as an expression/statement. Expressions need to evaluate to an integer. Statements return the altered state of the memory.

1. A `Var` expression needs to look the variable up in the current memory context, and retrieve its value.
2. A `Assign` statement needs to evaluate the expression in the current memory context, assign the returned value to the variable string, and return the resulting memory. This may shadow an existing value, and that is OK (or you may choose to remove any previous values, up to you).
3. A `Compound` expression computes each statement in turn, updating the memory as it goes along (a fold would do nicely for that), and finally computes the expression in the resulting memory context. Note that these changes to the memory only affect the computation of this expression.
4. A `Print` statement should evaluate the expression and use SML's `print` method to print the resulting value in its own line. If the expression is a `Var`, then it should print something like `x = ...`, otherwise it should print simply the resulting value.

Here is a skeleton for how the implementation might look like. It will be your task to fill in, most are rather easy.

    fun evalExp mem e =
       let val ev = evalExp mem
       in
          case e of
            Add (e1, e2)       => ...
          | Sub (e1, e2)       => ...
          | Mult (e1, e2)      => ...
          | Const i            => ...
          | Var s              => ...
          | Compound (stms, e) => ...
       end
    and evalStm mem stm =
       case stm of
         Assign (s, e)      => ...
       | Print (Var s)      => ...
       | Print e            => ...
    val eval = evalExp []    (* Shortcut for evaluation with empty memory. No need to change. *)

Here's one test of a successful implementation:

    (* Should return 6 *)
    val exp1 = eval (Compound ([Assign ("y", Add(Const 2, Const 3)), Print (Var "y")], Add (Var "y", Const 1)))

    (* Should return 5 *)
    val exp2 = eval (Compound ([Assign ("y", Const 2)], Add (Compound ([Assign ("y", Const 3), Print (Var "y")], Var "y"), Compound ([Print (Var "y")], Var "y"))))

Some extensions to consider:

1. Add a `PrintMem` statement, that prints the entire memory contents (but not shadowed variables).
2. Add a "value" datatype that holds the possible values of evaluation. A possible value would be either `IntV i` or `ErrorV s` where `s` is a string. Memory should be adjusted to hold `string * value` pairs, `evalExp` should be adjusted to return a `value`, and unsuccessful variable lookups would result in an `Error` value. Add division, where division by 0 would result in an appropriate `Error` value, as would non-perfect division (e.g. 5 / 3\. Try to create appropriate error messages for each instance).
3. Add boolean values, ways to compare/create boolean values, and if-then-else expressions. (As an easier start, you can implement `ifZero e1 then e2 else e3`)

### Simple stack-based calculator

This one is for more practice with pattern matching. We will build a simple stack-based calculator, where most operations pop elements at the top of the stack, perform a computation on them, and insert their results back into the stack.

A stack will simply be a list of integers:

    exception Empty
    type stack = int list
    empty: stack
    push: stack -> int -> stack
    pop: stack -> (i, stack)

Popping returns a pair of the value at the top of the stack and the remaining of the stack.

The datatype `Oper` holds the operations we can perform.

    datatype oper = Push of int | Pop | Add | Sub | Mult | Neg | Print

`Push i` adds the integer `i` onto the stack. `Pop` simply removes the top of the stack. `Add` takes out the two elements at the top of the stack, adds them and pushes the result back onto the stack. Similarly for `Sub` and `Mult` (where for `Sub` the number at the top is the negative one). `Neg` negates the value at the top of the stack. `Print` prints out in a new line the value at the top of the stack.

All of these operations should raise an `Empty` exception if there are not enough elements on the stack for them to operate.

The goal is of course to write a functions:

    evalOp: stack -> oper -> stack
    eval: oper list -> unit

Here `evalOp` evaluates a single operator with a given stack state, and returns the updated stack state, while `eval` takes a series of operations, and performs them on an empty stack, discarding the final result.

You can approach the problem in two ways. One is to rely solely on the `push/pop` functions defined earlier, and have those raise the appropriate exceptions when needed. The other is to utilize the implementation of the stack as a list, and to have a nested pattern match on the pair of the operation and the stack, like so:

    evalOp stk o =
       case (o, stk) of
         (Push i, _)          => ...
       | (Pop, _::stk')       => ...
       | (Add, i1::i2::stk')  => ...
       | (Sub, i1::i2::stk')  => ...
       | (Mult, i1::i2::stk') => ...
       | (Neg, i1::stk')      => ...
       | (Print, i1::_)       => ...
       | _                    => ...
