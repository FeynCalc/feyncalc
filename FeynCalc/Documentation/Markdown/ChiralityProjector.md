## ChiralityProjector

`ChiralityProjector[+1]` denotes $1/2\left(1+\gamma^5\right)$.

ChiralityProjector[-1] denotes $1/2\left(1+\gamma ^5\right)$.

The shortcut `ChiralityProjector` is deprecated, please use `GA[6]` and `GA[7]` instead!

### See also

[Overview](Extra/FeynCalc.md), [GA](GA.md), [FCI](FCI.md).

### Examples

```mathematica
{ChiralityProjector[+1], ChiralityProjector[-1]}
DiracSimplify[#, DiracSubstitute67 -> True] & /@ %
```

$$\left\{\bar{\gamma }^6,\bar{\gamma }^7\right\}$$

$$\left\{\frac{\bar{\gamma }^5}{2}+\frac{1}{2},\frac{1}{2}-\frac{\bar{\gamma }^5}{2}\right\}$$

`ChiralityProjector` is scheduled for removal in the future versions of FeynCalc. The safe alternative is to use GA[6] and GA[7].

```mathematica
{GA[6], GA[7]}
```

$$\left\{\bar{\gamma }^6,\bar{\gamma }^7\right\}$$

```mathematica
FCI[GA[6]] === ChiralityProjector[+1]
```

$$\text{True}$$

```mathematica
FCI[GA[7]] === ChiralityProjector[-1]
```

$$\text{True}$$
