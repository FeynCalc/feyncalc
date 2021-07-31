`NonCommQ[exp]` yields `True` if `exp` contains non-commutative objects (i.e. those objects which are listed in `$NonComm`) not inside `DiracTrace`s or `SUNTrace`s.

### See also

[$NonComm]($NonComm), [NonCommFreeQ](NonCommFreeQ), [DiracTrace](DiracTrace), [SUNTrace](SUNTrace).

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