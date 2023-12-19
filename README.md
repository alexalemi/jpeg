# JPEG

Janet PEG library in clojure.

The basic patterns include:

    `"cat"` (a string) - the literal string
    `3` (an integer) - Matches a given number of characters, if negative, matches only if there is not that many characters and consumes nothing, for example -1 will match the end of the string.
    `(range "az" "AZ")` - Matches characters in a range and advances 1 character. Multiple ranges can be combined.
    `(set "abcd")` - matches any in the set of characters and advances 1 character.
    
We can then use some combinators to make more patterns

    `(choice a b c ...)` - Tries to match a, then b, and so on. Will succeed on first match and fails if none of the arguments match.
    `(+ a b c ...)` - Alias for `choice`
    `(sequence a b c ...)` - Tries to match a, b, c, etc in a sequence. If any of the arguments fail to match, the whole pattern fails.
    `(* a b c ...)` - Alias for `sequence`
