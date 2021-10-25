{-
Sequences using Balanced Trees
==============================
-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sequence where

{-
In this problem, you will reuse the ideas of balanced trees to develop a data
structure for appendable, indexable sequences.

This problem draws together ideas that you have seen on past homework
assignments and extends them with problems about defining functor, applicative
and monad operations for list-like structures.

-}

{-
For this problem, you can use any function in the Control.Applicative and Control.Monad
libraries. You can
also import additional libraries from [base](https://hackage.haskell.org/package/base-4.14.2.0),
if desired.  However, our solution works with no additional imports.
-}

import Control.Applicative (Alternative (..))
import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import Data.Foldable (toList)
import qualified Data.Maybe as Maybe
import Test.HUnit hiding (State)
import Test.QuickCheck

{-
Sequences
---------

A sequence is a data structure, somewhat like a list, that supports
positional-based indexing of an ordered collection of elements.

Here is the interface that a sequence should satisfy.
-}

class (Monad l, Foldable l) => Sequence l where
  -- monoid-like operations
  nil :: l a
  append :: l a -> l a -> l a

  -- position based operations
  first :: l a -> Maybe a
  final :: l a -> Maybe a
  index :: Int -> l a -> Maybe a
  insert :: Int -> a -> l a -> Maybe (l a)

{-
Note that `Sequence` is a subclass of `Monad` and `Foldable`. Any type
constructor that is an instance of `Sequence` will also have to be an instance
of both of these two classes.

Sequences are like Lists
------------------------

With the `Monad` constraint, sequences act a lot like lists.
For example, we can define our favorite list operations using the
members of these type classes (or their superclasses).
-}

-- | Singleton sequence.
singleton :: Sequence l => a -> l a
singleton = pure

-- | Add an element to the beginning of the sequence.
cons :: Sequence l => a -> l a -> l a
cons x xs = pure x `append` xs

{-
Because sequences are similar to lists, we can also use these
operations to convert from regular lists to sequences. Fill in that
definition below.  (The analogous operation,
`toList` is already defined for any instance of `Foldable`).
-}

-- | Conversion from lists.
fromList :: Sequence l => [a] -> l a
fromList = undefined

{-
And, `Sequences` are monads, so our old friend `pairs` works for them too.
-}

-- | All pairs of elements in sequences `xs` and `ys`, in lexicographic order.
pairs :: Sequence l => l a -> l b -> l (a, b)
pairs = Monad.liftM2 (,)

{-
Lists are Sequences
-------------------

It won't come as a surprise that a normal list can implement the sequence interface.
Note that in `index` and `insert` below, if the position is out of range, the
result is `Nothing`.  (Note: we're deliberately using the partial functions
`head`, `last` and `(!!)` to demonstrate how the `guard` operation works with
the `Maybe` monad. Make sure that you understand how this code works!)
-}

instance Sequence [] where
  nil = []
  append = (<>)
  first l = Monad.guard (not (null l)) >> return (head l)
  final l = Monad.guard (not (null l)) >> return (last l)
  index n l = Monad.guard (0 <= n && n < length l) >> return (l !! n)
  insert n x l = Monad.guard (0 <= n && n <= length l) >> return (before ++ x : after)
    where
      (before, after) = splitAt n l

{-
However, indexing operations are inefficient for lists. Although `first` is
constant time, all of the other operations take time `O(n)` in the worst case,
where `n` is the length of the first list argument.

We can do better.

Balanced-tree Sequences
-----------------------

Consider the following AVL-tree inspired data structure for sequences. A `Seq`
is either `Empty`, or it is an `AVL` binary tree structure that stores data at
only its leaves. The `Branch` constructor includes the cached height of the
tree (so that we can rebalance) and the cached length of the sequence (so that
we can efficiently index).
-}

data Seq a
  = Empty -- Empty structure
  | AVL (AVL a) -- Non-empty tree structure w/ data at leaves
  deriving (Show)

data AVL a
  = Single a -- an element
  | Branch
      Int -- Cached number of elements
      Int -- Cached height (for balancing)
      (AVL a) -- Left child
      (AVL a) -- Right child
  deriving (Show)

{-
Note that accessing the height of the tree is a constant time operation.
-}

height :: AVL a -> Int
height (Single _) = 0
height (Branch _ k _ _) = k

{-
For example, here is an example AVL-based sequence, containing the numbers
`7, 3, 4` in that order.
-}

seq1 :: Seq Int
seq1 = AVL $ Branch 3 2 (Branch 2 1 (Single 7) (Single 3)) (Single 4)

{-
As part of this problem, you will implement the following functions, as well
 as complete an instance of the following classes: `Semigroup`,`Monoid`,
 `Foldable` and `Monad` for the `Seq` type.
-}

instance Sequence Seq where
  nil = Empty
  append = seqAppend
  first = seqFirst
  final = seqFinal
  index = seqIndex
  insert = seqInsert

{-
For example, here is a test case that you should be able to satisfy by the end
of the assignment.
-}

testPairs :: Test
testPairs =
  "pairs" ~: toList (pairs seq1 seq1)
    ~=? [(7, 7), (7, 3), (7, 4), (3, 7), (3, 3), (3, 4), (4, 7), (4, 3), (4, 4)]

-- (a) first and final

{-
AVL trees trade constant time "head" access for a `O(lg n)` running time for
all other operations. Here, accessing either the `first` or `last` element may
take time `O(lg n)`.
-}

-- | access the first element of the sequence, if there is one.
seqFirst :: Seq a -> Maybe a
seqFirst = undefined

-- | access the last element of the list, if there is one (similar to above)
seqFinal :: Seq a -> Maybe a
seqFinal = undefined

{-
>
-}

testFirst :: Test
testFirst =
  TestList
    [ "first" ~: first seq1 ~=? Just 7,
      "final" ~: final seq1 ~=? Just 4
    ]

-- (b) Reducing sequences

{-
The
[`Foldable`](https://hackage.haskell.org/package/base-4.12.2.0/docs/Data-Foldable.html)
type class allows us to treat sequences like lists when it comes to reducing
them to values. We can make an instance of this class merely by providing a
definition of the `foldr` function; all other operations, such as `length` are given
default definitions in terms of `foldr`.
-}

instance Foldable AVL where
  -- The default definition of the length function looks something like this:
  length = foldr (\x s -> s + 1) 0

  -- Override this definition with an optimized version that is O(1)
  {-
  >
  -}

  -- Finish the `foldr` definition below so that it is O(n) (Hint: see HW2)
  foldr f b (Single x) = f x b
  foldr f b (Branch _ _ xs ys) = undefined

instance Foldable Seq where
  -- The default definition of the length function looks something like this:
  length = foldr (\x s -> s + 1) 0

  -- Override this definition with an optimized version that is O(1)

  foldr _ b Empty = b
  foldr f b (AVL t) = foldr f b t

{-
We use the `toList` function to implement the equality function for this
type. We only care about the *sequence* of values that appear, not the tree
structure.
-}

instance Eq a => Eq (Seq a) where
  l1 == l2 = toList l1 == toList l2

testFoldable :: Test
testFoldable =
  TestList
    [ "length" ~: length seq1 ~?= 3,
      "toList" ~: toList seq1 ~?= [7, 3, 4],
      "sum" ~: sum seq1 ~?= 14
    ]

-- (c)  Indexing

{-
We use the stored length to navigate the tree structure when we reference an
element in the list by its index. Position `0` is the element at the head of
the sequence, counting up to `length-1`. If the given index is not in range,
this function should return `Nothing`. It should run in `O(lg n)` time.
-}

seqIndex :: Int -> Seq a -> Maybe a
seqIndex = undefined

testSeqIndex :: Test
testSeqIndex =
  TestList
    [ "index 0" ~: seqIndex 0 seq1 ~?= Just 7,
      "index 1" ~: seqIndex 1 seq1 ~?= Just 3,
      "index 2" ~: seqIndex 2 seq1 ~?= Just 4,
      "index 3" ~: seqIndex 3 seq1 ~?= Nothing
    ]

-- (d) Insert

{-
Next, adapt the AVL insertion function (and all of its dependencies) from your
previous homework to enable insertion into this structure. If you did not
successfully complete the AVL assignment, the TAs will show you the solution
during office hours.
-}

seqInsert :: Int -> a -> Seq a -> Maybe (Seq a)
seqInsert = undefined

{-
This test case checks that the value is inserted at the correct position,
but not whether the result is balanced.
-}

testSeqInsert :: Test
testSeqInsert =
  TestList
    [ "insert 0 " ~: toList <$> insert 0 1 seq1 ~?= Just [1, 7, 3, 4],
      "insert 1 " ~: toList <$> insert 1 1 seq1 ~?= Just [7, 1, 3, 4],
      "insert 2 " ~: toList <$> insert 2 1 seq1 ~?= Just [7, 3, 1, 4],
      "insert 3 " ~: toList <$> insert 3 1 seq1 ~?= Just [7, 3, 4, 1],
      "insert 4 " ~: toList <$> insert 4 1 seq1 ~?= Nothing
    ]

{-
We'll make sure that our trees stay balanced with QuickCheck.
-}

-- (e) Testing with QuickCheck

{-
Let's make some random sequences for testing!

Complete the `Arbitrary` instance, making sure you use the functions above to
construct arbitrary `AVL`s. Note: if you use `Branch` in the definition of
`arbitrary` your generated sequence may not be balanced. We want to only
generate *balanced* trees.
-}

instance (Show a, Arbitrary a) => Arbitrary (Seq a) where
  arbitrary = undefined
  shrink _ = undefined

{-
Now we can compare the stored sizes of random lists with ones where we have
explicitly counted every branch.
-}

prop_length :: Seq Int -> Bool
prop_length xs = Maybe.isJust (count xs)
  where
    count Empty = Just 0
    count (AVL t) = aux t
      where
        aux (Single _) = Just 1
        aux (Branch j _ l r) = do
          cl <- aux l
          cr <- aux r
          Monad.guard (j == cl + cr)
          return j

{-
Make sure that the heights are correctly calculated.
-}

prop_height :: Seq Int -> Bool
prop_height xs = Maybe.isJust (count xs)
  where
    count Empty = Just 0
    count (AVL t) = aux t
      where
        aux (Single _) = Just 0
        aux (Branch _ k l r) = do
          cl <- aux l
          cr <- aux r
          Monad.guard (k == 1 + max cl cr)
          return k

{-
And make sure that our sequences *stay* balanced.
-}

prop_balanced :: Seq Int -> Bool
prop_balanced Empty = True
prop_balanced (AVL t0) = aux t0
  where
    aux (Single _) = True
    aux t@(Branch _ _ l r) =
      bf t >= -1 && bf t <= 1 && aux l && aux r

-- the balance factor
bf :: AVL a -> Int
bf (Branch _ _ l r) = height l - height r
bf (Single _) = 0

{-
All three representation invariants together.
-}

prop_AVL :: Seq Int -> Property
prop_AVL x =
  counterexample "length" (prop_length x)
    .&&. counterexample "height" (prop_height x)
    .&&. counterexample "balanced" (prop_balanced x)

{-
And we can make sure that our AVL trees are still valid after
every insert.
-}

prop_insert_AVL :: Seq Int -> Int -> Property
prop_insert_AVL s x = forAll (choose (0, length s)) $ \i ->
  case seqInsert i x s of
    Just s' -> prop_AVL s'
    Nothing -> property False

-- (f) Semigroup and Monoid

instance Semigroup (Seq a) where
  (<>) = seqAppend

instance Monoid (Seq a) where
  mempty = Empty

{-
The beauty of this representation is that not only do we get efficient
indexing, we also can append two sequences together in `O(lg n)` time.

The general idea of the `append a b` function is that if the heights of `a` and
`b` are within 1 of eachother, put them together with the `branch`
constructor.  Otherwise, if `a` is taller than `b`, then look along the right
spine of `a` for a branch that is balanced with `b`.  At that point, construct
a new branch in the tree. However, that part of the tree is now one taller
than before, so it should be rebalanced on the way up. (The case when `b` is
taller than `a` is analogous.)
-}

seqAppend :: Seq a -> Seq a -> Seq a
seqAppend = undefined

{-
Be sure to make sure that append acts like the similar operation on lists
-}

prop_append :: Seq Int -> Seq Int -> Bool
prop_append l1 l2 = toList (l1 <> l2) == toList l1 ++ toList l2

{-
and produces balanced sequences.
-}

prop_append_SEQ :: Seq Int -> Seq Int -> Property
prop_append_SEQ l1 l2 = prop_AVL (seqAppend l1 l2)

-- (g) Functors and Monads (at last!)

{-
Like lists, this type can be made an instance of the `Functor`, `Applicative`
and `Monad` type classes. Fill in the details for `Functor` and `Monad` (we
have given you the definition of `Applicative`, which uses the monadic
operations).  You may find the `Monad` instance for ordinary lists to be a
useful inspiration. But, do *not* convert `Seq` trees to ordinary lists in your
solution!
-}

instance Functor Seq where
  fmap _ _ = undefined

instance Applicative Seq where
  pure = AVL . Single
  (<*>) = Monad.ap -- this function is defined in terms of bind

instance Monad Seq where
  return = undefined
  _ >>= _ = undefined

{-
How do you know that your `Functor` and `Monad` instances are
correct?  Type classes often come with *laws* that govern their correct
usage. For example, all implementations of `(==)` should be reflexive,
symmetric, and transitive. Instances that do not follow these laws are
confusing and unpredictable, leading to buggy programs.

Let's now write some QuickCheck properties to verify the `Functor` and `Monad`
laws. Instead of `a -> b`, we will use the datatype `Fun a b`, which allows
QuickCheck to generate arbitrary function values.  You do not need to
understand the details of this, but, if you're interested, you can watch [Koen
Claessen's talk](http://www.youtube.com/watch?v=CH8UQJiv9Q4) for background on
testing higher-order functions with QuickCheck.

Inside a property depending on a function `rf :: Fun a b`, we can get
the underlying "real" function `f :: a -> b` by pattern matching with
`(Fun _ f)`.

Functor instances should satisfy the two *laws* shown below.

The first law states that mapping the identity function shouldn't do anything.
-}

prop_FMapId :: (Eq (f a), Functor f) => f a -> Bool
prop_FMapId x = fmap id x == id x

{-
The second law allows us to combine two passes with `fmap` into a single one
using function composition.
-}

prop_FMapComp :: (Eq (f c), Functor f) => Fun b c -> Fun a b -> f a -> Bool
prop_FMapComp (Fun _ f) (Fun _ g) x =
  fmap (f . g) x == (fmap f . fmap g) x

{-
Furthermore, monad instances should satisfy the *three* monad laws given
below.
-}

prop_LeftUnit :: (Eq (m b), Monad m) => a -> Fun a (m b) -> Bool
prop_LeftUnit x (Fun _ f) =
  (return x >>= f) == f x

prop_RightUnit :: (Eq (m b), Monad m) => m b -> Bool
prop_RightUnit m =
  (m >>= return) == m

prop_Assoc ::
  (Eq (m c), Monad m) =>
  m a ->
  Fun a (m b) ->
  Fun b (m c) ->
  Bool
prop_Assoc m (Fun _ f) (Fun _ g) =
  ((m >>= f) >>= g) == (m >>= \x -> f x >>= g)

{-
Finally, types that are instances of both `Functor` and `Monad` should
satisfy one additional law:
-}

prop_FunctorMonad :: (Eq (m b), Monad m) => m a -> Fun a b -> Bool
prop_FunctorMonad x (Fun _ f) = fmap f x == (x >>= return . f)

{-

Now use QuickCheck to verify these properties for your `Functor` and `Monad`
instances above.

After you have completed the instances, make sure that your code
satisfies the properties by running the following computations.
-}

qc1 :: IO ()
qc1 = quickCheck (prop_FMapId :: Seq Int -> Bool)

qc2 :: IO ()
qc2 =
  quickCheck
    (prop_FMapComp :: Fun Int Int -> Fun Int Int -> Seq Int -> Bool)

qc3 :: IO ()
qc3 = quickCheck (prop_LeftUnit :: Int -> Fun Int (Seq Int) -> Bool)

qc4 :: IO ()
qc4 = quickCheck (prop_RightUnit :: Seq Int -> Bool)

-- warning, this one is slower than the rest. It takes 10-15 seconds on my machine.
qc5 :: IO ()
qc5 =
  quickCheck
    (prop_Assoc :: Seq Int -> Fun Int (Seq Int) -> Fun Int (Seq Int) -> Bool)

qc6 :: IO ()
qc6 = quickCheck (prop_FunctorMonad :: Seq Int -> Fun Int (Seq Int) -> Bool)

{-
Furthermore, the `Functor` and `Monad` instances for sequences should be
*equivalent* to the ones for ordinary lists. More formally, we require
following list equalities to hold, no matter what values are used for
`f`, `s`, `x`, `m`, and `k`.

    toList (fmap f s) == fmap f (toList s)
        where s :: Seq a
              f :: a -> b

    toList (return x) == return x
        where x :: a

    toList (m >>= k) == toList m >>= (toList . k)
        where m :: Seq a
              k :: a -> Seq b

Use QuickCheck to test that these three identities hold.
-}

qc7 :: IO ()
qc7 = undefined

qc8 :: IO ()
qc8 = undefined

qc9 :: IO ()
qc9 = undefined

{-
Finally, the `Functor` and `Monad` instances for `Seq` should preserve
the Seq invariants.
-}

qc10 :: IO ()
qc10 = quickCheck prop_Seq_functor
  where
    prop_Seq_functor :: Fun Int Int -> Seq Int -> Property
    prop_Seq_functor (Fun _ f) x = prop_AVL (fmap f x)

qc11 :: IO ()
qc11 = quickCheck prop_Seq_return
  where
    prop_Seq_return :: Int -> Property
    prop_Seq_return x = prop_AVL (return x)

qc12 :: IO ()
qc12 = quickCheck prop_Seq_bind
  where
    prop_Seq_bind :: Seq Int -> Fun Int (Seq Int) -> Property
    prop_Seq_bind x (Fun _ k) = prop_AVL (x >>= k)

{-
-- Make sure that you add qc7, qc8, and qc9 to this testing
-- function after you have defined them.
-}

qcSeq :: IO ()
qcSeq = qc1 >> qc2 >> qc3 >> qc4 >> qc5 >> qc6 >> qc10 >> qc11 >> qc12

-- (e)

{-
Now let's think about instances of `Functor` and `Monad` for `Seq`
that do _not_ satisfy the laws above. As a trivial example, if we
merely left all of the methods undefined, then quickCheck should
easily return a counterexample.  (You might want to verify that it
does!)
-}

{- Invalid instance of Functor and Monad:

instance Functor Seq where
    fmap f s = undefined
instance Monad Seq where
    return = undefined
    (>>=)  = undefined
-}

{-
Are there other invalid instances?  Add at least one instance below
(in comments) that does *not* use `undefined` or `error`, and does not
include an infinite loop. Your instance(s) should typecheck, but
should fail at least one of the tests above.  Please include a note
saying which property or properties are violated.
-}

{-
Homework Notes
--------------

This problem is inspired by Haskell's
[Data.Sequence](https://hackage.haskell.org/package/containers-0.5.10.2/docs/Data-Sequence.html)
library.  That library uses a data structure called FingerTrees, which is also
based on balanced binary trees, but include additional structure.  In
particular, FingerTrees provides amortized constant time `cons` and `head` and
operations. Furthermore, FingerTrees are more general: besides sequences they
can also be used to implement priority queues.

If you would like to learn more about FingerTrees, I recommend the following [talk](https://www.youtube.com/watch?v=ip92VMpf_-A&ab_channel=ACMSIGPLAN) by Koen Classen,
one of the inventers of the QuickCheck library.
-}

runTests :: IO ()
runTests = do
  _ <-
    runTestTT $
      TestList
        [ testPairs,
          testFirst,
          testFoldable,
          testSeqIndex,
          testSeqInsert
        ]
  quickCheck prop_AVL
  quickCheck prop_append
  quickCheck prop_append_SEQ
  qcSeq
