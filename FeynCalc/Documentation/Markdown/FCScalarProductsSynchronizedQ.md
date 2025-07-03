## FCScalarProductsSynchronizedQ

`FCScalarProductsSynchronizedQ[]` compares up and down values of scalar products
and other kinematic-related symbols such as `Momentum`, `CartesianMomentum`, `TC` etc.
between the master kernel and the subkernels and returns `True` if all of them identical.

This routine is only relevant in the parallel mode of FeynCalc. It helps to avoid inconsistencies through definitions that were introduced before activating the parallel mode and not correctly propagated to the subkernels

### See also

[Overview](Extra/FeynCalc.md), [ScalarProduct](ScalarProduct.md), [Pair](Pair.md), [SP](SP.md), [SPD](SPD.md).

### Examples

```mathematica
FCScalarProductsSynchronizedQ[]
```

![1baac185a4bv4](img/1baac185a4bv4.svg)

$$\text{True}$$