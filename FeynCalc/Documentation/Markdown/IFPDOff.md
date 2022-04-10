## IFPDOff

`IFPDOff[exp_, q1, q2, ...]` changes from `IFPD` representation to `FeynAmpDenominator[...]`. The `q1, q2, ...` are the integration momenta.

### See also

[Overview](Extra/FeynCalc.md), [IFPD](IFPD.md), [IFPDOn](IFPDOn.md).

### Examples

```mathematica
IFPD[Momentum[p], m]
```

$$(\overline{p}^2\text{ - }m^2)$$

```mathematica
IFPD[Momentum[p], m] // StandardForm

(*IFPD[Momentum[p], m]*)
```

```mathematica
ex = IFPDOff[IFPD[Momentum[p], m], p]
```

$$\overline{p}^2-m^2$$

```mathematica
ex // StandardForm

(*-m^2 + Pair[Momentum[p], Momentum[p]]*)
```