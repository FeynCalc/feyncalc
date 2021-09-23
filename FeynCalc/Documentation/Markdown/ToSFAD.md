## ToSFAD

`ToSFAD[exp]` converts all propagator denominators written as `FAD` or `FeynAmpDenmoninator[...,PropagatorDenominator[...],...]` to `SFAD` or `FeynAmpDenmoninator[...,StandardPropagatorDenominator[...],...]` respectively.

### See also

[Overview](Extra/FeynCalc.md), [FAD](FAD.md), [SFAD](SFAD.md).

### Examples

```mathematica
ToSFAD[FAD[p]]
% // StandardForm
```

$$\frac{1}{(p^2+i \eta )}$$

```
(*SFAD[{{p, 0}, {0, 1}, 1}]*)
```

```mathematica
ToSFAD[FAD[{p, m}]]
% // StandardForm
```

$$\frac{1}{(p^2-m^2+i \eta )}$$

```
(*SFAD[{{p, 0}, {m^2, 1}, 1}]*)
```

```mathematica
ToSFAD[FAD[{p + q, m, 2}]]
% // StandardForm

```

$$\frac{1}{((p+q)^2-m^2+i \eta )^2}$$

```
(*SFAD[{{p + q, 0}, {m^2, 1}, 1}, {{p + q, 0}, {m^2, 1}, 1}]*)
```
