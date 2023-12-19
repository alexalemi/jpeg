# TODO

Things I need to implement

 * add tests for default-grammar
 * `(backmatch ? tag)` - if tag is provided, matches against the tagged capture. If no tag is provided, matches against the last capture, but only if that capture is untagged. The peg advances if there was a match.
 
Capturing stuff.

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
