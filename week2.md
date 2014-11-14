
#### Last week's problems

The [list of the problems I posted last week](https://class.coursera.org/proglang-003/forum/thread?thread_id=300) have more elegant pattern-matching versions and are worth revisiting or doing for the first time.

#### Library functions

Another worthwhile exercise is to look at the functions defined in the [Option](http://sml-family.org/Basis/option.html) and [List](http://sml-family.org/Basis/list.html) structures
and to try to write these functions directly. Almost all of them have simple forms involving pattern-matching and recursion.

#### The Natural Numbers

Here is an extra set of problems using a custom datatype.

We define custom data type `nat` representing "the natural" numbers, 0, 1, 2, ... A "natural" number is either zero, or the "successor" of a another integer. So we can define them as follows:

    datatype nat = ZERO | SUCC of nat

So for example the number 1 is just "`SUCC ZERO`", the number 2 is "\`SUCC (SUCC ZERO)", and so on. Write the following functions:

1. `is_positive : nat -> bool`, which given a "natural number" returns whether that number is positive (i.e. not zero)
2. `succ : nat -> nat`, which given a "natural number" returns its successor (i.e. succ n = n+1).
3. `pred : nat -> nat`, which given a "natural number" returns its predecessor. Since 0 does not have a predecessor in the natural numbers, throw an exception `Negative` (will need to define it first).
4. `nat_to_int : nat -> int`, which given a "natural number" returns the corresponding integer in normal form. For example `nat_to_int (SUCC (SUCC ZERO)) = 2`.
5. `int_to_nat : int -> nat` which given an integer returns a "natural number" representation for it, or throws a `Negative` exception if the integer was negative. For the remaining problems, do not use these two functions
6. `add : nat * nat -> nat` that given two natural numbers, adds them. Do NOT convert them to integers to do that.
7. `sub : nat * nat -> nat` that given two natural numbers n, m computes the difference n-m. Do NOT convert them to integers to do that.
8. `mult : nat * nat -> nat` that given two natural numbers n, m computes the product n\*m. Do NOT convert them to integers to do that.
9. `is_less : nat * nat -> bool` that given two natural numbers returns whether the first one is less than the second. Do NOT convert them to integers to do that.

#### Set algebra

We define a "set of integers" as the following datatype:

    datatype intSet = Elems of int list                  (* A list of integers (possibly with duplicates to be ignored) *)
                    | Range of { from : int, to : int }  (* all integers from one number till another *)
                    | Union of intSet * intSet           (* Symbolically define the union of two sets *)
                    | Intersection of intSet * intSet    (* Symbolically define the intersection of two sets *)
                    | Difference of intSet * intSet      (* Difference: The elements in the first set that are not in the second *)

Consider the last clause there to be "extra". You may want to write the functions without it there, later tack it on if you feel like it.

Write the following functions:

1. `isEmpty : intSet -> bool` that determines if the set is empty or not
2. `contains: intSet * int -> bool` that returns whether the set contains a certain element or not
3. `insert: intSet * int -> intSet` that returns a new set with a new element inserted
4. `union: intSet * intSet -> intSet` that returns the union of two sets (you can approach this in multiple ways)
5. `intersect: intSet * intSet -> intSet` that returns the intersection of two sets (you can approach this in multiple ways)
6. `toList: intSet -> int list` that returns a sorted list of the elements in the set (with no duplicates)
