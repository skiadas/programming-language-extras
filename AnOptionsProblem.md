It seems some people are struggling a fair bit coming to terms with the option types, and in particular the `all_except_option` function. I decided to write a "small" article on it, that turned out to be not that small after all. I hope it helps some folks.

## Our friend the option type

It seems the option type is very intimidating for a lot of students when they first encounter it, so I will try in this short piece to demystify it a little bit.

This post will have three parts:

1. We will start with a brief refresher and explanation on what the option type is, and how the basic structure of working with it feels like.
2. We will then look at the standard library functions for option types, and we will implement them ourselves with case expressions.
3. We will wrap up with a worked out more complicated example showing how to work with recursive functions that return an option type.

Let's get started!

### Options options options

Let us start with the definition of the option type:

```
datatype 'a option = NONE | SOME of 'a
```

Hm, heavy stuff. This is also typically people's first introduction to datatypes, and it can get confusing. What this single line is doing is a number of things:

1. It creates a mysterious new "option" type, which is further parameterized by a type variable `'a`. So technically I lied a moment ago, "option" itself isn't a type, but we can have `int option`, `string option`, `string list option`, `(int * int) option` and so on. So we have __new kinds of values__. This is important, so let me emphasize it: We have new kinds of values! How do we write these new kinds of functions, you ask? A very, very good question. So let's answer it:
2. To the right of the equal sign, separated by `|`, we see various "constructors". i.e. ways of creating values. For instance we can write `NONE`, and that is a value of option type (OK for those pedantic among you that statement is only 80% correct). So single constructors like that are effectively just values of that datatype.
3. There's also the cool `SOME of 'a` part. What this does is it creates a function `SOME: 'a -> 'a option`. This is a weird function because it doesn't really evaluate in any way. But what it does tell you is that if `x` is of type `'a`, then you can write `SOME x` and it will be of type `'a option`.
4. And this is it. There are NO other ways of getting values of option type. They will be either `NONE`, or `SOME v` for an appropriate value `v`.
5. In order to do something with a value of option type, there is really only one tool at our disposal, a case expression, and we will see a number of examples in a moment. Everything else is just syntactic sugar on top of this case expression.

So what good are option types? Why do we bother with this bizarre construct? There are many answers, but the bottom line is: The option type can naturally express the presence or absence of a value. For example, suppose you have a number `n`, and you want to see if it is divisible by another number `m`. If it is divisible, then we want to compute the quotient. How can we express the result of this operation? I suppose if `m` does not divide `n` we could raise some sort of exception, but that seems awefully drastic: After all, all that poor number `m` did wrong was not divide `n`; It hardly seems worth interrupting the normal flow of operation for that.

This is where option types come in:

> An option type expresses the result of an operation which may have failed. `NONE` indicates a failed operation, while `SOME v` indicates a successful one.

Let's see how that would play out with our numbers. Here's our beautiful function:

```
val maybe_divide: int * int -> int option
fun maybe_divide (n, m) =
   if n mod m = 0
   then SOME (n div m)
   else NONE
```

So now we can safely write `maybe_divide (10, 3)` without having to worry about raising some sort of exception. Cool, right?

Alright so this is how we can produce a value of option type. Now how do we use such a value?

Let's continue with our previous example. I now want to write a function that given two numbers tries to divide them, and prints a message accordingly. Let's see how this might go. We will of course want to use our `maybe_divide` function, which returns an `int option` value. We will do a case expression on it:

```
val try_divide: int * int -> unit
fun try_divide (n, m) =
   case maybe_divide (n, m) of
     NONE    => print ("Was not able to divide " ^ Int.toString n ^ " by " ^ Int.toString m ^ "\n")
   | SOME d  => print ("Dividing " ^ Int.toString n ^ " by " ^ Int.toString m ^ " gives " ^ Int.toString d ^ "\n")
```

Try it out and see how it works!

This is it. Really. Now you know all there is to know about options types.

Okay fine, let's do some more examples. But really, this is it.

### The standard library for options

The basic library functions for options can be found in the [Option structure page](http://sml-family.org/Basis/option.html). Let's have a look at them:

```
datatype 'a option = NONE | SOME of 'a
exception Option

val getOpt : 'a option * 'a -> 'a
val isSome : 'a option -> bool
val valOf : 'a option -> 'a
val filter : ('a -> bool) -> 'a -> 'a option
val join : 'a option option -> 'a option
val app : ('a -> unit) -> 'a option -> unit
val map : ('a -> 'b) -> 'a option -> 'b option
val mapPartial : ('a -> 'b option) -> 'a option -> 'b option
val compose : ('a -> 'b) * ('c -> 'a option) -> 'c -> 'b option
val composePartial : ('a -> 'b option) * ('c -> 'a option) -> 'c -> 'b option
```

Okay let's take a closer look. The datatype definition we already saw. Then there is also a declaration of an exception to use for errors we cannot recover from. Then a number of function types follows. We will implement most of these functions.

The first function, `getOpt` is quite interesting. It takes two arguments: an `'a option` and an `'a`, and returns an `'a`. It does something quite simple: If the option was a `SOME v`, then it returns that `v`. Otherwise it returns the second value. So `getOpt (SOME [1,2], []) = [1, 2]` and `getOpt (NONE, 1) = 1`. Think of it as having a "default" value. Either you provided a value in your option, and then we'll use that, or you didn't and we'll use this other default value.

Here's an implementation of `getOpt`:
```
fun getOpt (a_opt, a') =
   case a_opt of
     NONE   => a'
   | SOME a => a
```

Let's move to the second function `isSome`. This takes an option value, and returns a boolean telling you if it is a `SOME ...` or not. Pretty straightforward, so much so that you might want to do it yourselves before reading on:
```
fun isSome a_opt =
   case a_opt of
     NONE   => false
   | SOME _ => true
```
Notice that we're using an underscore as a wildcard, because we don't actually care what value is there, just that there is something there.

Moving on, `valOf`. This takes the value out of the option if there is a value there. Otherwise it raises an error:
```
fun valOf a_opt =
   case a_opt of
     NONE   => raise Option
   | SOME v => v
```

I will skip most of the others for now, as they involve material on higher-order-functions, so you may want to revisit the page and think about them after you have learned about higher-order-functions. For now, I will focus on one more function, `join`. The type of the input to `join` seems weird: It is `'a option option'. It is an option type whose values are themselves option types. Woah! Maybe we'll need an example. Here's one: `SOME (SOME 5)` would be an `int option option`. `SOME NONE` would be another one. Or even just `NONE` itself. In fact these are the only cases. What `join` does is basically simplify this into one level:
```
fun join a_opt =
   case a_opt of
     NONE          => NONE
   | SOME NONE     => NONE
   | SOME (SOME v) => SOME v
```

This was a rather exhaustive look at all 3 cases. There are at least 2 other ways to write this function:
```
fun join a_opt =
   case a_opt of
     NONE       => NONE
   | SOME v_opt => v_opt

fun join a_opt = getOpt(a_opt, NONE)
```
Make sure you understand that last formulation.

Okay enough with looking at the basis library. Let's look at a bigger example.

### A bigger example: Factorization

Here is the problem we will tackle in this session. We are given two pieces of information. First, a number `n`. Then a list `primes` of prime numbers. The goal is to try to see if we can write the number `n` as a product of the numbers from the list. If it is not possible, we are to return `NONE`. If it is possible, we are to return `SOME lst`, where `lst` comprises the list of all the factors we would use. For example: `factorize (20, [2, 3, 5]) = SOME [2, 2, 5]`, while `factorize (20, [2, 3]) = NONE`.

Okay let us start with our function's signature. It needs to take a pair of an `int` and an `int list`, and it is to return an options list so:

```
factorize: int * int list -> int list option
```

Let's work through the steps in our problem:

1. The main recursive step should be to try the first number in the list, and see if it divides perfectly into `n`. If it does, then maybe we can divide it in, and then continue with the smaller `n`. If it does not, then we can discard it and move on to the next number in the list.
2. There are two reasons the process might end. One is if we reach the number `n = 1` we are done, we don't need to look for any more factors. This is a successful end to the story.
3. The other is if we run out of numbers to try, i.e. when the list of "primes" is empty. That is an unsuccessful end to the story.

Before doing code let us think of how this would play out in the examples we mentioned earlier:

```
work on                    (20, [2, 3, 5])       <--- Try 2 first. It fits
remember [2]   and work on (10, [2, 3 ,5])       <--- Try 2 again. It fits
remember [2,2] and work on (5, [2, 3 ,5])        <--- Try 2 again. It does not fit
remember [2,2] and work on (5, [3 ,5])           <--- Try 3. It does not fit
remember [2,2] and work on (5, [5])              <--- Try 5. It fits
remember [2,2] and work on (1, [5])              <--- Reached 1. End of story


work on                    (20, [2, 3])       <--- Try 2 first. It fits
remember [2]   and work on (10, [2, 3])       <--- Try 2 again. It fits
remember [2,2] and work on (5, [2, 3])        <--- Try 2 again. It does not fit
remember [2,2] and work on (5, [3])           <--- Try 3. It does not fit
remember [2,2] and work on (5, [])            <--- No more primes in the list. Problem!
```

Okay so hopefully those two examples gave you a rough idea of how this might go. Let's start thinking how we will structure the code. Let us start with our first line:

```
fun factorize (n, primes) =
```

Phew, OK, the hard part is done, let us rest for a second.

So, I will actually do this in two different ways. In the first approach, I will look at each of the two terminating conditions. And the easiest one to work with is the `n = 1` case. so that sounds like a good candidate for an `if-then-else`:

```
fun factorize (n, primes) =
   if n = 1
   then ??? 
   else ???
```

Hm, ok, what goes in the question marks? One thing is for sure, they need to be of type `int list option`, because that's what my function was supposed to return.

 Let's see, I've asked it to write `1` as a product of other numbers, so a sort of "[empty product](http://en.wikipedia.org/wiki/Empty_product)" of no terms at all would do it. So it makes sense to report success, so we must return a `SOME` of an int list, and since we don't need to use any factors we would return a `SOME []`. So we would now have:

```
fun factorize (n, primes) =
   if n = 1
   then SOME []
   else ???
```

Okay, what should happen in the else case then? This is the case where `n` is bigger than `1` (I'm assuming people give us positive integers, you can modify it if you will to handle negative integers), and so we must try the next prime in the list and see if it divides into `n`. So we need to peak into our `primes` list. We need a case expression for that, there is no other way around it (if we're not allowed to use `null`, `hd`, `tl`, and in general it is a somewhat bad idea to use them so try to avoid it). So now our function will look like this:

```
fun factorize (n, primes) =
   if n = 1
   then SOME []
   else case primes of
          []           =>  ???
        | m :: primes' =>  ???
```

Oh boy, more question marks. But again these are points where our function would terminate and return, so the values in those question marks again better be `int list option`s.

Let us start with the first case. This is the ase where we ran out of numbers to use. So we must report a failure. That's what `NONE` was designed to do. So let's do that:

```
fun factorize (n, primes) =
   if n = 1
   then SOME []
   else case primes of
          []           =>  NONE
        | m :: primes' =>  ???
```

In the other case, we must try to see if `m` divides `n`. This calls for another `if`:

```
fun factorize (n, primes) =
   if n = 1
   then SOME []
   else case primes of
          []           =>  NONE
        | m :: primes' =>  if n mod m = 0
                           then ???
                           else ???
```

Now at this point our function starts getting complicated enough that we might consider taking some parts of it apart into helper functions, and we will do that a bit later on. For now let us proceed with this sort of "brute force" approach.

What should happen in the `then` clause? This is the case where that factor `m` does divide `n`. So what we need to do is divide `m` into `n`, and then recursively call ourselves. So we will end up doing a `factorize (n div m, primes)` call. This call will return a `int list option` saying either `NONE` (can't be done) or `SOME lst` (can be done, and here are the remaining factors you will have to use). What we need to do now is handle these case. How, you ask? Well, with another case expression of course!

```
fun factorize (n, primes) =
   if n = 1
   then SOME []
   else case primes of
          []           =>  NONE
        | m :: primes' =>  if n mod m = 0
                           then case factorize (n div m, primes) of
                                  NONE     => ???
                                | SOME lst => ???
                           else ???
```

Hm we seem to just be getting more and more of these question marks. Do not fear though, we are almost done! Notice that these new question marks are still points where the original function would terminate. So they too need to return an `int list option`.

Before moving on, note that the recursive call is using `primes`, not `primes'`, as we need to retry the factor `m` and see if it fits even more times.

So, the `NONE` clause means that the remaining task of factorizing `n div m` cannot be done. Well that means our original problem cannot be done either, so we need to return `NONE`.

What about the `SOME lst` clause? Well that now tells us that the term `n div m` has been successfully factorized into a product of the terms in `lst`. All we need to do is add `m` to that list, so `m :: lst`. And that is a successful way of factorizing `n` now, so we need to return a `SOME` of that. So now we have:

```
fun factorize (n, primes) =
   if n = 1
   then SOME []
   else case primes of
          []           =>  NONE
        | m :: primes' =>  if n mod m = 0
                           then case factorize (n div m, primes) of
                                  NONE     => NONE
                                | SOME lst => SOME (m :: lst)
                           else ???
```

Phew, one last case to handle, and I promise you it's an easy one. What happens if `m` does not divide `n`? Then we simply need to move on to the remaining prime factors, and see if they are enough to handle `n`. So that is just a recursive call:

```
fun factorize (n, primes) =
   if n = 1
   then SOME []
   else case primes of
          []           =>  NONE
        | m :: primes' =>  if n mod m = 0
                           then case factorize (n div m, primes) of
                                  NONE     => NONE
                                | SOME lst => SOME (m :: lst)
                           else factorize (n, primes')
```

Phew! We are done, for now. Take a break, stretch your legs, and try this function out:

```
factorize(10, [2, 3, 5, 7])           ---->  SOME [2, 5]
factorize(20, [2, 3, 5, 7])           ---->  SOME [2, 2, 5]
factorize(11, [2, 3, 5, 7])           ---->  NONE
factorize(14, [2, 3, 5, 7])           ---->  SOME [2, 7]
```

When you are ready for some more cool stuff, just keep reading, as we are about to simplify our function, and take it from 9 lines down to 5.

### Simplifying our factorization function

There are two key ways in which we can simplify our function.

1. Some of the nested `if/case` clauses can some times be merged into one `case` on a nested pattern, when the conditions they are testing do not depend on each other.
2. Some functionality that seems complicated may look better when singled out into a helper function.

We will perform these two steps now. First, look at the two first branching points. One depends on the value of `n`, the other depends on the list `primes`. Those can become one:

```
fun factorize (n, primes) =
   case (n, primes) of
     (1,  _)           => SOME []
   | (_,  [])          => NONE
   | (_, m :: primes') => if n mod m = 0
                          then case factorize (n div m, primes) of
                                 NONE     => NONE
                               | SOME lst => SOME (m :: lst)
                          else factorize (n, primes')
```

You may prefer this version to the original one, or you may find it more confusing. But in any case it is an option worth keeping in mind.

The second simplification we can make has to do with the inner case expression. It seems to do something fairly intuitive. It has a list option and an element, and if that list option is `NONE` then it just maintains that `NONE` (I like to think of this as "propagating the failures"), while if it is a `SOME lst` then it prepends that element to that list before re-wrapping the whole thing in a `SOME`. This step has nothing to do with primes, numbers, divisibility, it is a nice isolated property of list options, so let's make it into its own function `cons_opt: 'a * 'a list option -> 'a list option`:

```
fun cons_opt (x, xs_opt) =
   case xs_opt of
     NONE    => NONE
   | SOME xs => SOME (x :: xs)
```

With higher-order-functions we could take this one step futher, but for now it will do. In either case, this is a key idea of functional programming, and programming in general, to isolate these reusable pieces of functionality into their own functions.

Let us rework our `factorize` function to use our new and awesome `cons_opt`:

```
fun factorize (n, primes) =
   case (n, primes) of
     (1,  _)           => SOME []
   | (_,  [])          => NONE
   | (_, m :: primes') => if n mod m = 0
                          then cons_opt (m, factorize (n div m, primes))
                          else factorize (n, primes')
```

There, nice and sweet!

And by now you must surely have become an expert in using option types. May it serve you well in your future endeavors.

Good luck!