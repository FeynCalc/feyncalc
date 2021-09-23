## NonCommQ

`NonCommQ[exp]` yields `True` if `exp` contains non-commutative objects (i.e. those objects which are listed in `$NonComm`) not inside `DiracTrace`s or `SUNTrace`s.

### See also

[Overview](Extra/FeynCalc.md), [$NonComm]($NonComm.md), [NonCommFreeQ](NonCommFreeQ.md), [DiracTrace](DiracTrace.md), [SUNTrace](SUNTrace.md).

### Examples

```mathematica
NonCommQ[xx + yy]
```

$$\text{False}$$

```mathematica
NonCommQ[GA[\[Mu]] . GS[p + m] . GA[\[Mu]]]
```

$$\text{True}$$

```mathematica
NonCommQ[DCHN[GA[\[Mu]], i, j]]
```

$$\text{True}$$
