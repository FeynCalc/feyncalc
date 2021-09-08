## FCReplaceD

`FCReplaceD[expr, rule]` replaces `D` in expr according to the supplied replacement rule (e.g. `D -> 4 - 2*Epsilon`) but doesn't touch `D` inside `Pair`s and `DiracGamma`s, i.e the dimension of scalar products, metric tensors and Dirac matrices is unchanged. The latter can and should be done via `ChangeDimension`.

### See also

[Overview](Extra/FeynCalc.md).

### Examples

Applying the replacement rule directly to the expression doesn't give the desired result

```mathematica
FCI[D MTD[\[Mu], \[Nu]]]
% /. D -> 4 - 2 Epsilon
```

$$D g^{\mu \nu }$$

$$(4-2 \varepsilon ) g_{\{4-2 \varepsilon ,4-2 \varepsilon \}}{}^{\mu \nu }$$

With `FCReplaceD` we get what we want

```mathematica
FCReplaceD[D MTD[\[Mu], \[Nu]], D -> 4 - 2 Epsilon]
ChangeDimension[%, 4]
```

$$(4-2 \varepsilon ) g^{\mu \nu }$$

$$(4-2 \varepsilon ) \bar{g}^{\mu \nu }$$