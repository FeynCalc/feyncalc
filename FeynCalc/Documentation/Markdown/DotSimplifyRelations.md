## DotSimplifyRelations

`DotSimplifyRelations` is an option for `DotSimplify`. Its setting may be a list of substitution rules of the form `DotSimplifyRelations -> {a.b -> c, b^2 -> 0, ...}`.

In the rules, `Condition` should not be used and patterns should be avoided on the right-hand sides.

Notice that the performance of `DotSimplify` scales very badly with the complexity of `DotSimplifyRelations` and the number of terms of the expression.

### See also

[Overview](Extra/FeynCalc.md), [DotSimplify](DotSimplify.md).

### Examples
