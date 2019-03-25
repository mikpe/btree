# btree
B-tree storage of sets with configurable I/O.

This implements B-trees of any order >= 2 in Erlang.

I/O is handled by user-provided callbacks.

Only sets (keys without data) are supported.

## Implementation notes

- Initially based on [Wirth76].
- Converted from Pascal to C, and then to Erlang.  The Erlang
  version uses tuples in lieu of arrays.  Pages are of variable
  not fixed size.
- I/O of pages is made explicit.
- Search is separated from insertion.  Search returns a stack of
  pages and indices recording the path from the root to the key.
  Insertion uses that stack to navigate the tree when rewriting it.
- Deletion is tricky enough that the recursive structure of the
  original code is kept as-is.  A local cache is used to avoid
  redundant I/O.
- Our use cases only want sets, so this does not associate
  attributes with the keys.
- Bulk-loading into an empty tree has been added.

## References

[BM72] R. Bayer and E. McCreight, "Organization and Maintenance of
Large Ordered Indexes", Acta Informatica, Vol. 1, Fasc. 3, pp. 173--189,
1972.

[Wirth76] Niklaus Wirth, "Algorithms + Data Structures = Programs",
Program 4.7, pp 252--257, Prentice-Hall, 1976.

[Comer79] Douglas Comer, "The Ubiquitous B-Tree", Computing Surveys,
Vol. 11, No. 2, pp 121--137, ACM, June 1979.
