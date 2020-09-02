# hitchhiker Prolog

An implementation of a novel Prolog virtual machine,
proposed by Paul Tarau.

See http://www.cse.unt.edu/~tarau/research/2017/eng.pdf for details.

Ported to Javascript, C++ and Pascal by CapelliC.

---------

Some interesting properties, from the above document abstract:

  * the heap representation of terms and the abstract machine instruction encodings are the same
  * no dedicated code area is used as the code is placed directly on the heap
  * unification and indexing operations are orthogonal
  * filtering of matching clauses happens without building new structures on the heap
  * variables in function and predicate symbol positions are handled with no performance penalty
  * a simple English-like syntax is used as an intermediate representation for clauses and goals
  * the same English-like syntax can be used by programmers directly as an alternative to classic Prolog syntax
  * solutions of (multiple) logic engines are exposed as answer streams that can be combined through typical functional programming patterns
  * performance of a basic interpreter implementing our design is within a factor of 2 of a highly optimized WAM-based system

--------

Ported to plain Javascript, simplifying somewhat the code.
Running in Firefox, performance are about 1/3 of Java 8.

Several data structures have been replaced by arrays constructs:
  IntList, IntStack, IntMap, ObStack

Available to run in browsers or NodeJS.

--------

Ported to C++ from Javascript, with small optimizations.
Slightly faster than Java 8.

Most data structures have been replaced by std::vector:
  IntList, IntStack, IntMap, ObStack

--------

Ported to Free Pascal from C++.
Performance still to be evaluated.

Most data structures have been adapted from Generics.
The recursive data structure used for term interfacing has been redesigned
and a little optimized.
