## EpsExpand

`EpsExpand` is an option for `EpsEvaluate` and other functions that use `EpsEvaluate` internally. When set to `False`, sums of momenta in the `Eps` tensor will not be rewritten as a sum of `Eps` tensors.

### See also

[Overview](Extra/FeynCalc.md), [EpsEvaluate](EpsEvaluate.md).

### Examples

```mathematica
LC[mu, nu][q1 + q2, p1 + p2]
EpsEvaluate[%]
```

$$\bar{\epsilon }^{\text{mu}\;\text{nu}\;\overline{\text{q1}}+\overline{\text{q2}}\;\overline{\text{p1}}+\overline{\text{p2}}}$$

$$-\bar{\epsilon }^{\text{mu}\;\text{nu}\;\overline{\text{p1}}\;\overline{\text{q1}}}-\bar{\epsilon }^{\text{mu}\;\text{nu}\;\overline{\text{p1}}\;\overline{\text{q2}}}-\bar{\epsilon }^{\text{mu}\;\text{nu}\;\overline{\text{p2}}\;\overline{\text{q1}}}-\bar{\epsilon }^{\text{mu}\;\text{nu}\;\overline{\text{p2}}\;\overline{\text{q2}}}$$

```mathematica
LC[mu, nu][q1 + q2, p1 + p2]
EpsEvaluate[%, EpsExpand -> False] 
  
 

```

$$\bar{\epsilon }^{\text{mu}\;\text{nu}\;\overline{\text{q1}}+\overline{\text{q2}}\;\overline{\text{p1}}+\overline{\text{p2}}}$$

$$-\bar{\epsilon }^{\text{mu}\;\text{nu}\;\overline{\text{p1}}+\overline{\text{p2}}\;\overline{\text{q1}}+\overline{\text{q2}}}$$
