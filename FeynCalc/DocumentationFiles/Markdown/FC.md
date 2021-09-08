## FC

`FC` changes the output format to `FeynCalcForm`. To change to `InputForm` use `FI`.

### See also

[Overview](Extra/FeynCalc.md), [FeynCalcForm](FeynCalcForm.md), [FI](FI.md), [FeynCalcExternal](FeynCalcExternal.md), [FeynCalcInternal](FeynCalcInternal.md).

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
