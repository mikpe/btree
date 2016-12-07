# btree
B-tree storage of sets with configurable I/O.

This implements B-trees of any order >= 2 in Erlang.

I/O is handled by user-provided callbacks.  One possibility
is to embed a B-tree inside another storage structure.

Only sets (keys without data) are supported.
