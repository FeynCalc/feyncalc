## DiracOrder

`DiracOrder[exp]` orders the Dirac matrices in `exp` lexicographically. `DiracOrder[exp, orderlist]` orders the Dirac matrices in `exp` according to `orderlist`. `DiracOrder` is also an option of `DiracSimplify` and some other functions dealing with Dirac algebra. If set to `True`, the function `DiracOrder` will be applied to the intermediate result to reorder the Dirac matrices lexicographically.

### See also

[Overview](Extra/FeynCalc.md), [DiracSimplify](DiracSimplify.md), [DiracTrick](DiracTrick.md).

### Examples

```mathematica
GA[\[Beta], \[Alpha]]
DiracOrder[%]
```

$$\bar{\gamma }^{\beta }.\bar{\gamma }^{\alpha }$$

$$2 \bar{g}^{\alpha \beta }-\bar{\gamma }^{\alpha }.\bar{\gamma }^{\beta }$$

`DiracOrder` also works with Dirac matrices in  $D$-dimensions.

```mathematica
GAD[\[Rho], \[Nu], \[Mu], \[Nu]]
DiracOrder[%]
```

$$\gamma ^{\rho }.\gamma ^{\nu }.\gamma ^{\mu }.\gamma ^{\nu }$$

$$(D-2) \gamma ^{\mu }.\gamma ^{\rho }+2 (2-D) g^{\mu \rho }$$

By default $\gamma^5$ is moved to the right.

```mathematica
GA[5, \[Mu], \[Nu]]
DiracOrder[%]
```

$$\bar{\gamma }^5.\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }$$

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^5$$

```mathematica
GA[6, \[Mu], 7]
DiracOrder[%]
```

$$\bar{\gamma }^6.\bar{\gamma }^{\mu }.\bar{\gamma }^7$$

$$\bar{\gamma }^{\mu }.\bar{\gamma }^7$$

`orderlist` comes into play when we need an ordering that is not lexicographic

```mathematica
GA[\[Alpha], \[Beta], \[Delta]]
DiracOrder[%]
```

$$\bar{\gamma }^{\alpha }.\bar{\gamma }^{\beta }.\bar{\gamma }^{\delta }$$

$$\bar{\gamma }^{\alpha }.\bar{\gamma }^{\beta }.\bar{\gamma }^{\delta }$$

```mathematica
DiracOrder[GA[\[Alpha], \[Beta], \[Delta]], {\[Delta], \[Beta], \[Alpha]}]
```

$$-\bar{\gamma }^{\delta }.\bar{\gamma }^{\beta }.\bar{\gamma }^{\alpha }+2 \bar{\gamma }^{\delta } \bar{g}^{\alpha \beta }-2 \bar{\gamma }^{\beta } \bar{g}^{\alpha \delta }+2 \bar{\gamma }^{\alpha } \bar{g}^{\beta \delta }$$

Reordering of Dirac matrices in long chains is expensive, so that `DiracSimplify` does not do it by default.

```mathematica
DiracSimplify[GAD[\[Mu], \[Nu]] + GAD[\[Nu], \[Mu]]]
```

$$\gamma ^{\mu }.\gamma ^{\nu }+\gamma ^{\nu }.\gamma ^{\mu }$$

However, if you know that it can lead to simpler expressions, you can activate the reordering via the option `DiracOrder`.

```mathematica
DiracSimplify[GAD[\[Mu], \[Nu]] + GAD[\[Nu], \[Mu]], DiracOrder -> True]
```

$$2 g^{\mu \nu }$$
