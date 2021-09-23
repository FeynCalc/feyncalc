## NonCommHeadQ

`NonCommHeadQ[exp]` yields `True` if the head of exp is a non-commutative object or `Dot`.

### See also

[Overview](Extra/FeynCalc.md), [DataType](DataType.md), [DeclareNonCommutative](DeclareNonCommutative.md), [UnDeclareNonCommutative](UnDeclareNonCommutative.md), [NonCommFreeQ](NonCommFreeQ.md), [NonCommQ](NonCommQ.md)

### Examples

```mathematica
NonCommHeadQ[GA[mu]]
```

$$\text{True}$$

```mathematica
NonCommHeadQ[GA[mu, nu, mu]]
```

$$\text{True}$$

```mathematica
NonCommHeadQ[FV[p, mu]]
```

$$\text{False}$$

```mathematica
NonCommHeadQ[FCI[SUNT[a]]]
```

$$\text{True}$$

```mathematica
NonCommHeadQ[FCI[SUNTF[a, i, j]]]
```

$$\text{False}$$
