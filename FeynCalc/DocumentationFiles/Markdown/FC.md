`FC` changes the output format to `FeynCalcForm`. To change to `InputForm` use `FI`.

### See also

[FeynCalcForm](FeynCalcForm), [FI](FI), [FeynCalcExternal](FeynCalcExternal), [FeynCalcInternal](FeynCalcInternal).

### Examples

```mathematica
FI
{DiracGamma[5], DiracGamma[Momentum[p]]}
```

```mathematica
{DiracGamma[5], DiracGamma[Momentum[p]]}
```

```mathematica
FC
{DiracGamma[5], DiracGamma[Momentum[p]]}
```

$$\left\{\bar{\gamma }^5,\bar{\gamma }\cdot \overline{p}\right\}$$