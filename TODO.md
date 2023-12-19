# TODO

Things I need to implement

 * add tests for default-grammar
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
 * `(backmatch ? tag)` - if tag is provided, matches against the tagged capture. If no tag is provided, matches against the last capture, but only if that capture is untagged. The peg advances if there was a match.
 * `(opt patt)` - alias for `(between 0 1 patt)`
 * `(? patt)` - alias for `(between 0 1 patt)`
 * `(n patt)` - alias for `(repeat n patt)`
 
Capturing stuff.

 * `(capture patt ?tag)` - captures all of the text in patt if patt matches. If patt contains any captures, then those captures will be pushed on to the capture stack before the total text.
 * `(<- patt ?tag)` - Alias for `(capture patt ?tag)`
 * `(quote patt ?tag)` - Another alias for `(capture patt ?tag)`
 * `(group patt ?tag)` - Captures an array of all of the captures in patt.
 * `(replace patt subst ?tag)` - Replaces the captures produced by patt by applying subst to them. If subst is a table or struct, will push `(get subst last-capture)` to the capture stack after removing the old capture. If subst is a function, will call subst with the captures of patt as arguments and push the result to the capture stack. Otherwise will push subst literally to the capture stack.
 * `(/ patt subst ?tag)` - Alias for `(replace patt subst ?tag)`
 * `(constant k ?tag)` - Captures a constant value and advances no characters.
 * `(argument n ?tag)` - Captures the nth extra argument to the match function and does not advance.
 * `(position ?tag)` - captures the current index into the text and advances no input.
 * `($ ?tag)` - Alias for `(position ?tag)`
 * `(column ?tag)` - Captures the column number of the current position in the matched text.
 * `(line ?tag)` - Captures the line number of the current position in the matched text.
 * ...

Also, try to implement the whole thing as a `trampoline` ...
