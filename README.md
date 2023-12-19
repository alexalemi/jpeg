# JPEG

Janet PEG library in clojure.

## Primitive Patterns

The basic patterns include:

 * `"cat"` (a string) - the literal string
 * `3` (an integer) - Matches a given number of characters, if negative, matches only if there is not that many characters and consumes nothing, for example -1 will match the end of the string.
 * `(range "az" "AZ")` - Matches characters in a range and advances 1 character. Multiple ranges can be combined.
 * `(set "abcd")` - matches any in the set of characters and advances 1 character.
    
## Combinators

We can then use some combinators to make more patterns

 * `(choice a b c ...)` - Tries to match a, then b, and so on. Will succeed on first match and fails if none of the arguments match.
 * `(+ a b c ...)` - Alias for `choice`
 * `(sequence a b c ...)` - Tries to match a, b, c, etc in a sequence. If any of the arguments fail to match, the whole pattern fails.
 * `(* a b c ...)` - Alias for `sequence`
 * `(any x)` - 0 or more repetitions
 * `(some x)` - 1 or more 
 * `(between min max x)` - matches between min and max (inclusive) repetitions of x.
 * `(at-least n x)` - matches at least n repetitions of x
 * `(at-most n x)` - matches at most n repetitions of x.
 * `(repeat n x)` - matches exactly n repetitions of x.
 
 * `(if cond patt)` - tries to match patt only if cond matches as well. cond will not produce any captures.
 * `(if-not cond patt)` - tries to match only if cond does not match. cond will not produce captures.

 * `(look offset patt)` - matches only if patt matches at a fixed offset. offset can be any integer. patt will not produce captures and the peg will not advance any characters.
 * `(> offset patt)` - alias for `(look offset patt)`
 
 * `(to patt)` - match up to patt (but not including it). If the end of the input is reached and patt is not matched, the entire pattern does not match.
 * `(thru patt)` - match up through patt (thus including it). If the end of the input is reached and patt is not matched, the entire pattern does not match.

 * `(opt patt)` - alias for `(between 0 1 patt)`
 * `(? patt)` - alias for `(between 0 1 patt)`
 * `(n patt)` - alias for `(repeat n patt)`
 
## Capturing

 * `(capture patt ?tag)` - captures all of the text in patt if patt matches. If patt contains any captures, then those captures will be pushed on to the capture stack before the total text.
 * `(<- patt ?tag)` - Alias for `(capture patt ?tag)`
 * `(quote patt ?tag)` - Another alias for `(capture patt ?tag)`
 * `(group patt ?tag)` - Captures an array of all of the captures in patt.
