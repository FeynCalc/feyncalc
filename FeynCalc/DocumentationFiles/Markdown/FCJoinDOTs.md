## FCJoinDOTs

`FCJoinDOTs` is an option for `DotSimplify` and other functions that use `DotSimplify` internally. When set to `True`, `DotSimplify` will try to rewrite expressions like `A.X.B + A.Y.B` as `A.(X+Y).B`.

Notice that although the default value of `FCJoinDOTs` is `True`, the corresponding transformations will occur only if the option `Expanding` is set to `False` (default: `True`)

### See also

[Overview](Extra/FeynCalc.md), [DotSimplify](DotSimplify.md).

### Examples

```mathematica
DeclareNonCommutative[A, B, X, Y]
```

```mathematica
DotSimplify[A . X . B + A . Y . B]
```

$$A.X.B+A.Y.B$$

```mathematica
DotSimplify[A . X . B + A . Y . B, FCJoinDOTs -> True]
```

$$A.X.B+A.Y.B$$

```mathematica
DotSimplify[A . X . B + A . Y . B, FCJoinDOTs -> True, Expanding -> False]
```

$$A.(X+Y).B$$

```mathematica
DotSimplify[GA[mu, 6, nu] + GA[mu, 7, nu], FCJoinDOTs -> True, Expanding -> False]
```

$$\bar{\gamma }^{\text{mu}}.\left(\bar{\gamma }^6+\bar{\gamma }^7\right).\bar{\gamma }^{\text{nu}}$$
