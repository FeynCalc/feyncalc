## ToSFAD

`ToSFAD[exp]` converts all propagator denominators written as `FAD` or `FeynAmpDenmoninator[...,PropagatorDenominator[...],...]` to `SFAD` or `FeynAmpDenmoninator[...,StandardPropagatorDenominator[...],...]` respectively.

### See also

[Overview](Extra/FeynCalc.md), [FAD](FAD.md), [SFAD](SFAD.md).

### Examples

```mathematica
ToSFAD[FAD[p]]
```

$$\frac{1}{(p^2+i \eta )}$$

```mathematica
ToSFAD[FAD[p]] // StandardForm

(*FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p, D], 0, 0, {1, 1}]]*)
```

```mathematica
ToSFAD[FAD[{p, m}]]
```

$$\frac{1}{(p^2-m^2+i \eta )}$$

```mathematica
ToSFAD[FAD[{p, m}]] // StandardForm

(*FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p, D], 0, -m^2, {1, 1}]]*)
```

```mathematica
ToSFAD[FAD[{p + q, m, 2}]]
```

$$\frac{1}{((p+q)^2-m^2+i \eta )^2}$$

```mathematica
ToSFAD[FAD[{p + q, m, 2}]] // StandardForm

(*FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p + q, D], 0, -m^2, {1, 1}], StandardPropagatorDenominator[Momentum[p + q, D], 0, -m^2, {1, 1}]]*)
```