##  IFPDOff 

IFPDOff[exp_, q1_, q2_, ...] changes from IFPD representation to FeynAmpDenominator[ ...]. The q1, q2, ... are the integration momenta..

###  See also 

IFPD, IFPDOn.

###  Examples 

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