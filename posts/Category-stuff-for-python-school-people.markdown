---
title: Cat content
date: 2014-01-01
---
Category stuff for python school people
=======================================

During the Python School in Zurich, some of the other tutors asked whether I could tell them, what that whole Monad stuff was all about, and if, in simple words, I could explain, what a Monad actually *is*.

Probably they were not terribly interested in an answer anyway but now they have a post.

Functors
--------

As a monad builds upon the definition of a functor, we’ll start with this one. A functor is easy to understand.

A functor gives you a mapping between one type and another which fulfills a few constraints.


A functor connects

We start with a simple list comprehension in Python:

~~~python
shopping_items = [(2, "apples"), (4, "bananas"), (6, "tomatoes")]

def duplicate(item):
    return (item[0] * 2, item[1])

def duplicate(item):
    factor = 2
    return (item[0] * factor, item[1])

def multiply(item, factor):
    return (item[0] * factor, item[1])

shopping_for_two = [(lambda i: multiply(i, 2))(i) for i in shopping_items]
>>> [(4, 'apples'), (8, 'bananas'), (12, 'tomatoes')]

def shopping_for_n(n):
    return [(lambda i: multiply(i, n))(i) for i in shopping_items]

my_list = [f(x) for x in other_l]
~~~

The following constraints hold (for *pure* functions):

~~~python
other_l = [x for x in other_l] # f = identity

[g(f(x)) for x in other_l] == [g(y) for y in [f(x) for x in other_l]]
~~~

Equivalent to this is the Python built-in `map` for any *iterable* it:

~~~python
map(lambda x: x, it) == it

map(lambda x: g(f(x)), it) = map(g, map(f, it))
~~~

A functor now is the generalised concept of this not constrained to iterables but to


Monads
------

### A programmable semicolon

Sometimes, monads are said to be a *programmable semicolon*[^wikipedia:monads]. What does this mean? And why a semicolon? Isn’t a semicolon meaningless in code?

[^wikipedia:monads]: <http://en.wikipedia.org/wiki/Monads_in_functional_programming>


If we look at the do notation, we understand what is meant by the term ‘programmable semicolon’.

~~~haskell
print >> print >> print
~~~

Problems with Pyhton’s list comprehensions???


Why is that at all important? Well, why is it important to have an iterator interface with __iter__ and __next__ methods and an appropriate stopping exception? Because it is something to abstract over for code re-use.

Applicative functors
--------------------


If-clauses
----------

## Error Handling
    l = []
    if not l:
        raise ValueException()
    else:
        do_stuff()

We could to

    try:
        r = do_stuff()
        a = do_more_stuff(r)
        return a
    except (SomeException, SomeOtherException) as e:
        return e




Uses
----

Validation. Instead of raising an exception, we can encode the possibility of an exception in our type information. This ensures that the calling code has to find some way to handle possible failure. In a way, this is similar to checked exceptions where all occuring exceptions need to be announced in the method definition and all code referring to a method must similarly either deal with the exception or announce it all the same. Exceptions, however checked or unchecked, are side-effects and not referentially transparent. We cannot store an excpetion inside a value. Validation is an expression which we can store in order to deal with it at a later point. Additionally, whatever representation we choose, we get applicative, monadic or monoidic behaviour.

Example:

    def check_args_a(a):
        try:
            return a[0]
        except IndexError:
            raise ArgumentException("Wrong argument a")

    # same for b

    (a, b) = args
    a = check_args_a(a)
    b = check_args_b(b)

    # raises immediately
    # alternatively:

    try:
        checked_a = check_args_a(a)
    except ArgumentException as ex:
        checked_a = None
        msg_a = ex.msg

    # ...
    # repetitive and similar to whats done automatically in validation

    def check_args_a(a: List[String]): Validation[NEL[String], Int] =
        try { a(0).success }
        catch { case IndexOutOfBoundsException => "Wrong argument a".failure }

    val (a, b) = args
    val checked_a = check_args_a(a)

    // later

    doWithAB()
    
## liftM is more than just list transformation




Generation
----------

    for { i <- arbitrary[Int] } yield i


On request: Comonads
--------------------

Comonads are not so commonly found but their name probably demands an explanation.

An example which makes use of the comonad’s methods can be found in the following example. Suppose we have a simple binary tree structure:

    data Tree a = Empty | Node a (Tree a) (Tree a)

with its functor instance defined as follows:

    instance Functor Tree where
      fmap f Empty = Empty
      fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

Suppose we now want to have access to additional state of the node’s children during the `fmap`, we find out that this is not easily doable. Also, the type of `fmap :: (a -> b) -> f a -> f b` clearly does only allow for access to `a` and not to `a left right`. Either we build our own alternative to `fmap` or we need a generic way of transforming `Tree a` to something like `Tree (a left right)` or `Tree (Tree a)`.

The function which does this is called `duplicate :: w a -> w (w a)`{.haskell}

```haskell
duplicate Empty = Empty
duplicate (Node a left right) = Node (Node a left right) (duplicate left) (duplicate right)

treesum Empty = 0
treesum (Node a left right) = a + (treesum left) + (treesum right)

fmap treesum (duplicate t)
```





