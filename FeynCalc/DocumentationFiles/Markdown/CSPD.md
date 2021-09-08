## CSPD

`CSPD[p, q]` is the $D-1$-dimensional scalar product of `p` with `q` and is transformed into `CartesianPair[CartesianMomentum[p, D-1],CartesianMomentum[q, D-1]]` by `FeynCalcInternal`.

`CSPD[p]` is the same as `CSPD[p,p]` ($=p^2$).

### See also

[Overview](Extra/FeynCalc.md), [SPD](SPD.md), [ScalarProduct](ScalarProduct.md), [CartesianScalarProduct](CartesianScalarProduct.md).

### Examples

```mathematica
CSPD[p, q] + CSPD[q]
```

$$p\cdot q+q^2$$

```mathematica
CSPD[p - q, q + 2 p]
```

$$(p-q)\cdot (2 p+q)$$

```mathematica
Calc[ CSPD[p - q, q + 2 p] ]
```

$$-p\cdot q+2 p^2-q^2$$

```mathematica
ExpandScalarProduct[CSPD[p - q]]
```

$$-2 (p\cdot q)+p^2+q^2$$

```mathematica
CSPD[a, b] // StandardForm

(*CSPD[a, b]*)
```

```mathematica
CSPD[a, b] // FCI // StandardForm

(*CartesianPair[CartesianMomentum[a, -1 + D], CartesianMomentum[b, -1 + D]]*)
```

```mathematica
CSPD[a, b] // FCI // FCE // StandardForm

(*CSPD[a, b]*)
```

```mathematica
FCE[ChangeDimension[CSP[p, q], D]] // StandardForm

(*CSPD[p, q]*)
```
