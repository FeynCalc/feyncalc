## IFPDOff

`IFPDOff[exp_, q1, q2, ...]` changes from `IFPD` representation to `FeynAmpDenominator[...]`. The `q1, q2, ...` are the integration momenta.

### See also

[Overview](Extra/FeynCalc.md), [IFPD](IFPD.md), [IFPDOn](IFPDOn.md).

### Examples

```mathematica
IFPD[Momentum[p], m]
% // StandardForm
```

$$(\overline{p}^2\text{ - }m^2)$$

```
(*IFPD[Momentum[p], m]*)
```

```mathematica
IFPDOff[%, p]
% // StandardForm
```

$$\overline{p}^2-m^2$$

```
(*-m^2 + Pair[Momentum[p], Momentum[p]]*)
```
