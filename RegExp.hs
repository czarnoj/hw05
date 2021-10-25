{-
Regular Expressions
===================
-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RegExp where

{-
In this problem, we'll start you off with some key library imports, but you can
also import additional libraries from [base](https://hackage.haskell.org/package/base-4.14.2.0),
if desired.  However, our solution works with no additional imports.
-}

-- Key data structures and libraries

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Maybe as Maybe
-- Haskell's standard implementation of Finite Sets.
import Data.Set (Set)
import qualified Data.Set as Set
-- Testing
import Test.HUnit hiding (State)
import Test.QuickCheck

{-
*Regular expressions* are a specialized language for representing
string-matching patterns. Regular expressions were invented by the
mathematician [Stephen Kleene](http://en.wikipedia.org/wiki/Stephen_Kleene),
one of the pioneers that created the foundations of the theory of
computation (with Goedel, Turing, Church, and Post). Kleene invented
regular expressions to represent the sets of possible behaviors of the
abstract finite computation devices called *finite-state machines*. In
Kleene's original formulation, regular expressions were were built
from individual letters with three operators: *concatenation*,
representing one pattern followed by another; *alternation* (also
called union) denoted by `|`, representing two alternative patterns;
and *closure* (also called Kleene-Star), denoted by `*`, to represent
zero or more repetitions of a pattern. By convention, the empty string
represents a special regular expression, the *empty* regular
expression, which matches the empty string.

For example, the regular expression `a(bc|d)*e` matches all strings
that start with `a`, then have some number of `bc` or `d` characters
(possibly none), and end with `e`. Some such strings include `ae`,
`abce`, `abcde`, `adbcde`, `abcbcdbce`. In the 1970s, computer
scientists at Bell Labs were working on the first software tools for
text processing, and they adapted and generalized Kleene's idea in
several software tools, starting with
[grep](http://en.wikipedia.org/wiki/Grep), for searching and editing
text. Regular expressions have many practical uses, mainly in pattern
matching, which has applications in everything from compilers to
searching databases.

In this problem, you will consider regular expression evaluators, that is,
programs that determine whether a string is in the language denoted by
the regular expression. This process is also called regular expression
matching.

We can represent regular expressions using the following datatype in Haskell.
-}

data RegExp
  = Char (Set Char) -- single literal character
  -- matches any character in the (nonempty) set
  | Alt RegExp RegExp -- r1|r2, alternation
  | Append RegExp RegExp -- r1 r2, concatenation
  | Star RegExp -- r*, Kleene star
  | Empty -- ε, accepts empty string
  | Void -- ∅, always fails
  deriving (Eq, Show)

{-
Your goal will be to define the following two functions:

           accept    :: RegExp -> String -> Bool
           match     :: RegExp -> String -> Bool

These functions use two different algorithms to determine whether the
given `String` is accepted by the given `RegExp`.

The point of this exercise is to give you practice with the list monad (in
the implementation of `accept`), an introduction to a really cool FP
algorithm (in the implmentation of `match`), and more practice with QuickCheck.

Characters and Character sets
-----------------------------

The first data constructor of the `RegExp` datatype defines a regular
expression that matches a *single* character drawn from some specified
set. For example, if we want to define a regexp that only matches the single
character `a`, we could write:
-}

-- | accept only the character 'a'
aRE :: RegExp
aRE = Char (Set.singleton 'a')

{-
When working with regexps, we can define some shorthand character classes to
make our lives easier, such as finite sets of digits, whitespace, lowercase,
uppercase characters.
-}

-- | Digits, usually written \d in regexp libraries
digit :: Set Char
digit = Set.fromList ['0' .. '9']

-- | Whitespace, usually written \s
white :: Set Char
white = Set.fromList " \n\r\t"

-- | lowercase \l
lower :: Set Char
lower = Set.fromList ['a' .. 'z']

-- | uppercase \u
upper :: Set Char
upper = Set.fromList ['A' .. 'Z']

-- | Word characters \w
word :: Set Char
word = upper <> lower <> digit <> Set.fromList "_"

-- | The union of all of the above, plus punctuation
-- but not including the newline characters
-- This set is usually referred to as '.' in regexp libraries
anyc :: Set Char
anyc = word <> Set.fromList " \t!@#$%^&*()_+{}|:\"<>?~`-=[]\\;',./"

----------------------------------------------------

{-
Examples and derived forms
--------------------------

A name is an upper case letter followed by a sequence of lowercase letters of
any length. In a regular expression library, we might write it as
`[A-Z][a-z]*`. (The `*` post-fix operator corresponds to 0 or more occurrences
of a pattern.)

Using our datatype above, we can express this regexp as:
-}

name :: RegExp
name = Char upper `Append` Star (Char lower)

testName :: Test
testName =
  "name"
    ~: TestList
      [ accept name "Stephanie" ~? "a good name",
        accept name "S" ~? "short is ok",
        not (accept name "stephanie") ~? "must be capitalized",
        not (accept name "Ste7anie") ~? "no extra symbols",
        not (accept name "Steph Annie") ~? "not even spaces"
      ]

{-
We can also define regexps that only accept specific words. For example,
given this function
-}

string :: String -> RegExp
string "" = Empty
string s = foldr1 Append (map (Char . Set.singleton) s)

{-
we can define this RegExp to recognize the name of our favorite course.
-}

cis552 :: RegExp
cis552 = string "cis552"

{-
We can also use `Star` and `Append` to define the `plus` operator, which
corresponds to one or more occurrences of a pattern.
-}

plus :: RegExp -> RegExp
plus pat = pat `Append` Star pat

{-
For example, this regular expression accepts any non-empty string that is
surrounded by the tags `<b>` and `</b>`.
-}

boldHtml :: RegExp
boldHtml = string "<b>" `Append` plus (Char anyc) `Append` string "</b>"

{-
Before you go further, note that we have provided you with a function, called
`display` that displays regexps succinctly (i.e. it tries to use
abbreviations such as \w and \s for character classes). Try comparing the
derived show instance with what this function does on the the `RegExp`s
above.
-}

-- >>> show boldHtml
-- "Append (Append (Append (Char (fromList \"<\")) (Append (Char (fromList \"b\")) (Char (fromList \">\")))) (Append (Char (fromList \"\\t !\\\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\")) (Star (Char (fromList \"\\t !\\\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\"))))) (Append (Char (fromList \"<\")) (Append (Char (fromList \"/\")) (Append (Char (fromList \"b\")) (Char (fromList \">\")))))"

-- >>> display boldHtml
-- "<b>(.+)</b>"

{-
RegExp Acceptance
-----------------

First, we'll implement a straightforward (and *extremely* inefficient) operation,
called `accept`, that determines whether a particular string is part of the
regular language accepted by the given regular expression.

Begin by implementing the following two helper functions. (For practice with
the list monad, you *must* use a list comprehension in these
implementations.)
-}

-- | all decompositions of a string into two different pieces
-- >>> -- split "abc"
-- [("","abc"),("a","bc"),("ab","c"),("abc","")]
split :: [a] -> [([a], [a])]
split = undefined

-- | all decompositions of a string into multi-part (nonempty) pieces
--
-- >>> -- parts "abc"
-- [["abc"],["a","bc"], ["ab","c"], ["a","b","c"]]
parts :: [a] -> [[[a]]]
parts = undefined

{-
Don't forget to write some quickCheck properties so that you can test
your functions! For example, here's one that makes sure that we get the
right number of pairs from split.
-}

prop_splitLength :: [Int] -> Bool
prop_splitLength l = length (split l) == length l + 1

{-
You can do better, though!
-}

-- Add a QuickCheck property for split
prop_split :: [Int] -> Bool
prop_split = undefined

{-
Note: we have to be careful when randomly testing parts; this is an
exponential algorithm after all. QuickCheck's `resize` function allows us to bound the
size of the lists that are produced so that they don't get too big. If you
find that `prop_partsLength` takes too long, you can decrease the argument to
`resize`.
-}

smallLists :: Gen [String]
smallLists = resize 15 arbitrary

prop_partsLength :: Property
prop_partsLength = forAll smallLists $ \l ->
  length (parts l) == 2 ^ predNat (length l)
  where
    predNat n = if n <= 0 then 0 else n - 1

-- Add a QuickCheck property for parts
prop_parts :: Property
prop_parts = undefined

{-
Now, use `split` and `parts` to determine whether a `RegExp` matches the given
input string. Your implementation should simply explore all
possibilities. For example, to determine whether the concatenation pattern
`Append r1 r2` matches an input string, use `split` to compute _all_ possible
ways of splitting the string into two parts and see whether `r1` and r2` match
the two parts. Again, use list comprehensions as part of the design of
your implementation.
-}

-- | Decide whether the given regexp matches the given string
accept :: RegExp -> String -> Bool
accept _ _ = undefined

testAccept :: Test
testAccept =
  "accept"
    ~: TestList
      [ not (accept Void "a") ~? "nothing is void",
        not (accept Void "") ~? "really, nothing is void",
        accept Empty "" ~? "accept Empty true",
        not (accept Empty "a") ~? "not accept Empty",
        accept (Char lower) "a" ~? "accept lower",
        not (accept (Char lower) "A") ~? "not accept lower",
        accept boldHtml "<b>cis552</b>!</b>" ~? "cis552!",
        not (accept boldHtml "<b>cis552</b>!</b") ~? "no trailing"
      ]

----------------------------------------------------

{-
QuickChecking `accept`
---------------------

How can we use quick check to test our implementation of the `accept`
function?

One idea is that we can use regular expressions in reverse---instead of using
them to *identify* strings from a language, we can instead use them to
*generate* strings from a language.

So, given a regexp, the `accept` function is correct if it returns true
for all strings generated from the language of the regexp.
-}

prop_accept :: Property
prop_accept = forAllShrinkShow arbitrary shrink display $ \r ->
  -- print counterexamples using 'display'
  case genRegExpString r of
    Just g -> forAll g $ accept r -- if we can generate a random string,
    -- then it should be accepted
    Nothing -> property $ isVoid r -- otherwise, we should have a RegExp
    -- equivalent to 'Void'

-- | Is this the regexp that never matches any string?
isVoid :: RegExp -> Bool
isVoid Void = True
isVoid (Append r1 r2) = isVoid r1 || isVoid r2
isVoid (Alt r1 r2) = isVoid r1 && isVoid r2
isVoid (Star _) = False -- can always match the empty string
isVoid (Char m) = Set.size m == 0
isVoid Empty = False

{-
Write a function that can generate a String that is accepted by the given
RegExp. Note that this function is partial; some RegExps accept *no* strings
and denote the empty language.

NOTE: in the case of `Star`, your function does not need to generate *all*
strings accepted by the RegExps. Instead, put a bound on the number of
iterations in your result. In particular, iterating more than twice could
cause a large blow-up when quickChecking `prop_accept`.
-}

-- | Create a generator for the strings accepted by this RegExp (if any)
genRegExpString :: RegExp -> Maybe (Gen String)
genRegExpString r = undefined

{-
Now complete an `Arbitrary` instance for regular expressions to test this
property above. Your regexps should only contain the characters "abcd".

Make sure that you also implement the 'shrink' function too.
-}

instance Arbitrary RegExp where
  arbitrary = undefined
  shrink = undefined

----------------------------------------------------

{-
Simplifying Regular Expressions
--------------------------------

You may notice that when working with regular expressions, there are several
ways to describe the same set of strings. For example, the regular expression

    a**|a**

which we would encode as
-}

astar :: RegExp
astar = Star (Star aRE) `Alt` Star (Star aRE)

{-
matches exactly the same set of strings as `a*`, i.e. strings composed of
any number of a's.

However, using the first regexp to test for acceptance takes a lot more time
than the second, especially when the string doesn't match. For example, I
found this comparison on my laptop:

     *RegExp> accept astar "aaaaaaaaaaaaaaab"
     False
     (5.87 secs, 4,278,663,696 bytes)
     *RegExp> accept (Star a) "aaaaaaaaaaaaaaab"
     False
     (0.07 secs, 50,671,104 bytes)

We can optimize code that works with RegExp through the use of "smart
constructors".  These smart constructor recognize simplifications that can be
made when ever a regular expression is put together. Suppose we have "smart"
variants of the *star* and *alt* regular expression constructors. Then we can
form the regexp in the same way, but (sometimes) get better performance.

     *RegExp> let astar' = star (star a) `alt` star (star a)
     (0.00 secs, 68,904 bytes)
     *RegExp> accept astar' "aaaaaaaaaaaaaaab"
     False
     (0.07 secs, 50,670,984 bytes)

For example, here is a definition of smart constructor for `star`. This
construct looks for simplifications that it can apply while constructing the
output.
-}

star :: RegExp -> RegExp
star r1 | isEmpty r1 = Empty
-- iterating the empty string is the empty string
star r1 | isVoid r1 = Empty
-- zero or more occurrences of void is empty
star (Star r) = Star r
-- two iterations is the same as one
star r = Star r

-- no optimization

-- | Is this the regexp that accepts *only* the empty string
isEmpty :: RegExp -> Bool
isEmpty Empty = True
isEmpty (Append r1 r2) = isEmpty r1 && isEmpty r2
isEmpty (Alt r1 r2) =
  (isEmpty r1 && isEmpty r2)
    || (isEmpty r1 && isVoid r2)
    || (isEmpty r2 && isVoid r1)
isEmpty (Star r) = isEmpty r || isVoid r
isEmpty (Char _) = False
isEmpty Void = False

{-
How do we know that our definition of `star` is really smart? We want to be
sure that the regexp that it produces matches the same strings as its input.

We'll compare the optimized version with the original to make sure that they
match the same strings. (We'll also make this a conditional property to make
sure that we only do the test when the smart constructor actually modifies the
string.)
-}

prop_star :: RegExp -> Property
prop_star r = sr /= Star r ==> sr %==% Star r
  where
    sr = star r

{-
We can quickCheck our optimizations using `accept` and the following property.
This property tests pairs of regexps on strings and ensures that either they
are both accepted or both rejected. To make this property more efficient, it
randomly selects strings that are either accepted by the first regexp,
accepted by the second, or that contain arbitrary sequences of a's, b's, c's,
and d's.
-}

-- | Property to determine whether two regexps accept the same language of
-- strings
(%==%) :: RegExp -> RegExp -> Property
r1 %==% r2 = forAll (genString r1 r2) $
  \s -> accept r1 s == accept r2 s
  where
    genString :: RegExp -> RegExp -> Gen String
    genString r1s r2s =
      oneof $
        Maybe.catMaybes
          [ genRegExpString r1s,
            genRegExpString r2s,
            Just $ resize 10 (listOf (elements "abcd"))
          ]

{-
Now design and test similar optimizations for sequencing and alternation. (For
inspiration, read the wikipedia page about [Kleene
Algebra](https://en.wikipedia.org/wiki/Kleene_algebra).)
-}

-- | Smart constructor for `Append`
append :: RegExp -> RegExp -> RegExp
append = undefined

prop_append :: RegExp -> RegExp -> Property
prop_append r1 r2 = rs /= Append r1 r2 ==> rs %==% Append r1 r2
  where
    rs = append r1 r2

-- | Smart constructor for `Alt`
alt :: RegExp -> RegExp -> RegExp
alt = undefined

{-
>
-}

prop_alt :: RegExp -> RegExp -> Property
prop_alt r1 r2 = rs /= Alt r1 r2 ==> rs %==% Alt r1 r2
  where
    rs = alt r1 r2

----------------------------------------------------

{-
Regular Expression Derivatives
------------------------------

You may have noticed by now that this implementation of Regular
Expression matching is *really* slow.  Let's fix that.

The textbook way to implement regular expression matching is to first
translate the regular expression into a finite-state machine and then
apply the finite-state matching to the string.

However, there's a more direct, elegant, but not so well-known
alternative, the method of *derivatives* due to Janusz
A. Brzozowski]. This method is
described in more detail
[here](http://sigfpe.blogspot.com/2005/05/derivatives-of-regular-expressions.html).

The basic idea is that, given a regular expression and the first
character in the input string to match, you can compute a new regular
expressions, which must match the remaining string in order for the
original `RegExp` to match the entire input string. This new regular
expression is called the *derivative* of the original.

We can use this idea to implement regular expression matching by
repeatedly calculating the derivatives for each character in the
string. If the final result is a regular expression that accepts the
empty string, then the original regular expression would have matched
the string. In other words:
-}

-- | Determine whether the given regexp matches the given String
match :: RegExp -> String -> Bool
match r s = nullable (List.foldl' deriv r s)

{-
Your job is to implement `nullable` and `deriv` to complete this
implementation.
-}

-- | `nullable r` return `True` when `r` could match the empty string
nullable :: RegExp -> Bool
nullable _ = undefined

-- |  Takes a regular expression `r` and a character `c`,
-- and computes a new regular expression that accepts word `w`
-- if `cw` is accepted by `r`. Make sure to use the smart constructors
-- above when you construct the new `RegExp`
deriv :: RegExp -> Char -> RegExp
deriv _ _ = undefined

{-
For example, if `r` is the literal character `c`, then the derivative of `r`
is `Empty` --- the regular expression that only accepts the empty string. On
the other hand, the derivative with respect to any *other* character is
`Void` --- `r` cannot match this character, so its derivative cannot match
anything. In the case of `Append`, you need to think about the case where the
first regular expression could accept the empty string. In that case, the
derivative should include the possibility that it could be skipped, and the
character consumed by the second regexp.

Note that Haskell's lazy evaluation avoids the evaluation of the whole
regular expression. The expression has only to be evaluated as much as
`nullable` needs to calculate an answer.
-}

-- Don't forget to test 'match' with HUnit and QuickCheck.

-------------------------------------------------

{-
Running the Tests
-----------------
-}

runTests :: IO ()
runTests = do
  putStrLn "Unit tests for name, accept"
  _ <- runTestTT $ TestList [testName, testAccept]
  putStrLn "Checking split/parts length"
  quickCheck prop_splitLength
  quickCheck prop_partsLength
  putStrLn "Checking split"
  quickCheck prop_split
  putStrLn "Checking accept"
  quickCheck prop_accept
  putStrLn "Checking star"
  quickCheck prop_star
  putStrLn "Checking append"
  quickCheck prop_append
  putStrLn "Checking alt"
  quickCheck prop_alt

{-
Displaying Regular Expressions
---------------------------

The following code is meant to help you display regular expressions. You do
not need to edit it.
-}

-- | display a set of characters succinctly, and escape all special characters
displayCharSet :: Set Char -> ShowS
displayCharSet s
  | s == lower = showString "\\l"
  | s == upper = showString "\\u"
  | s == digit = showString "\\d"
  | s == white = showString "\\s"
  | s == word = showString "\\w"
  | s == anyc = showString "."
  | Set.size s == 1 = showString (concatMap escape (Set.elems s))
  | otherwise =
    showString "["
      . showString (concatMap escape (Set.elems s))
      . showString "]"

-- | Add slashes in front of standard regexp characters
escape :: Char -> String
escape c
  | c `elem` "[\\^$.|?*+()" = ['\\', c]
  | otherwise = [c]

-- | Display a regexp, using precedence to reduce the number of required
-- parentheses in the output.
display :: RegExp -> String
display r = displayPrec 0 r ""
  where
    -- special case for '+'
    displayPrec :: Int -> RegExp -> ShowS
    displayPrec p (Append r1 (Star r2))
      | r1 == r2 =
        showParen (p > 6) $ displayPrec 6 r1 . showString "+"
    displayPrec _ (Char s) = displayCharSet s
    displayPrec p (Alt r1 r2) =
      showParen (p > 7) $ displayPrec 7 r1 . showString "|" . displayPrec 7 r2
    displayPrec p (Append r1 r2) =
      showParen (p > 10) $ displayPrec 10 r1 . displayPrec 10 r2
    displayPrec p (Star r0) =
      showParen (p > 6) $ displayPrec 6 r0 . showString "*"
    displayPrec _ Empty = showString "ε"
    displayPrec _ Void = showString "∅"
