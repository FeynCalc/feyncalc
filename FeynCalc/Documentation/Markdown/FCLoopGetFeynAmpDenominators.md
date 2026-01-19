## FCLoopGetFeynAmpDenominators

`FCLoopGetFeynAmpDenominators[expr, {k1,k2,...}, head]` extracts all single propagator denominators present in the expression that depend on the loop momenta `k1, k2, ...`.

The function returns a list of two elements.  The first one contains the original expression with selected denominators wrapped with `head`. The second one is the list of relevant denominators

Setting the option `"Massless"` to `True`will select only massless denominators.

The option `Momentum` specifies the dependency on external momenta. When set to a list of momenta, relevant propagators will be selected irrespective of being massless or massive.

### See also

[Overview](Extra/FeynCalc.md), [FeynAmpDenominatorCombine](FeynAmpDenominatorCombine.md).

### Examples

```mathematica
amp = Get@FileNameJoin[{$FeynCalcDirectory, "Documentation", 
      "Examples", "Amplitudes", "Q-Q-massless-2L.m"}];
```

```mathematica
FCReloadFunctionFromFile[FCLoopGetFeynAmpDenominators]
```

All denominators depending on k1, k2

```mathematica
FCLoopGetFeynAmpDenominators[amp, {k1, k2}, denHead] // Last
```

$$\left\{\text{denHead}\left(\frac{1}{(\text{k1}^2+i \eta )}\right),\text{denHead}\left(\frac{1}{(\text{k1}^2-\text{mq}^2+i \eta )}\right),\text{denHead}\left(\frac{1}{(\text{k2}^2+i \eta )}\right),\text{denHead}\left(\frac{1}{(\text{k2}^2-\text{mq}^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k1}+\text{k2})^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k1}+\text{k2})^2-\text{mq}^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k1}-p)^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k1}-p)^2-\text{mq}^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k1}-\text{k2}-p)^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k1}-\text{k2}-p)^2-\text{mq}^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k2}-p)^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k2}-p)^2-\text{mq}^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k1}+p)^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((-\text{k1}+\text{k2}+p)^2+i \eta )}\right)\right\}$$

All denominators depending on k1, k2 and the external momentum p

```mathematica
FCLoopGetFeynAmpDenominators[amp, {k1, k2}, denHead, Momentum -> {p}] // Last
```

$$\left\{\text{denHead}\left(\frac{1}{((\text{k1}-p)^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k1}-p)^2-\text{mq}^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k1}-\text{k2}-p)^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k1}-\text{k2}-p)^2-\text{mq}^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k2}-p)^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k2}-p)^2-\text{mq}^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k1}+p)^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((-\text{k1}+\text{k2}+p)^2+i \eta )}\right)\right\}$$

All denominators depending on k1, k2 and the external momentum p as well as massless denominators

```mathematica
FCLoopGetFeynAmpDenominators[amp, {k1, k2}, denHead, Momentum -> {p}, "Massless" -> True] // Last
```

$$\left\{\text{denHead}\left(\frac{1}{(\text{k1}^2+i \eta )}\right),\text{denHead}\left(\frac{1}{(\text{k2}^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k1}+\text{k2})^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k1}-p)^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k1}-p)^2-\text{mq}^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k1}-\text{k2}-p)^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k1}-\text{k2}-p)^2-\text{mq}^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k2}-p)^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k2}-p)^2-\text{mq}^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((\text{k1}+p)^2+i \eta )}\right),\text{denHead}\left(\frac{1}{((-\text{k1}+\text{k2}+p)^2+i \eta )}\right)\right\}$$