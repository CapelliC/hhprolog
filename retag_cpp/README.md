retag_cpp
==========

Attempt a micro optimization, moving tag bits in upper tagged words
part, to avoid right shifting the numeric index that constitute
the main payload of the execution stack.

No gain has been observed, nevertheless the code is somewhat cleaner
because a more uniform tagged word has been introduced.
